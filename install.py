#!/usr/bin/env python3
"""
Apply NixOS or Home Manager configurations from a flake.

This script is a thin orchestration layer:
- Nix flakes are the source of truth
- Python handles platform detection and bootstrapping
- Shell usage is limited to invoking nix commands

It supports:
- NixOS systems via nixos-rebuild
- macOS and non-NixOS Linux via home-manager (run through `nix run`)
- Dynamic discovery of targets from flake outputs
"""

from __future__ import annotations

import argparse
import json
import os
import platform
import shutil
import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]


def run(cmd: list[str], *, cwd: Path | None = None) -> None:
    """
    Run a command and raise on failure.

    The command is echoed before execution for transparency.
    """
    print("+", " ".join(cmd))
    subprocess.run(cmd, cwd=cwd, check=True)


def capture(cmd: list[str], *, cwd: Path | None = None) -> str:
    """
    Run a command and return its stdout as text.

    The command is echoed before execution.
    """
    print("+", " ".join(cmd))
    return subprocess.check_output(cmd, cwd=cwd, text=True)


def have(exe: str) -> bool:
    """
    Return True if an executable is available in PATH.
    """
    return shutil.which(exe) is not None


def is_wsl() -> bool:
    """
    Detect whether the current system is running under WSL.
    """
    if os.environ.get("WSL_DISTRO_NAME"):
        return True
    try:
        return "microsoft" in Path("/proc/version").read_text().lower()
    except Exception:
        return False


def bash_login(cmd: str) -> list[str]:
    """
    Wrap a command to be executed in a login shell.

    This ensures that the Nix profile is sourced, which is required
    immediately after installing Nix.
    """
    return ["bash", "-lc", cmd]


def ensure_nix() -> None:
    """
    Ensure that the `nix` command is available.

    If Nix is not installed, it will be bootstrapped using:
    - Determinate Systems installer on macOS
    - Official Nix installer (daemon mode) on Linux
    """
    if have("nix"):
        return

    system = platform.system()
    print(f"Nix not found — bootstrapping on {system}")

    if system == "Darwin":
        run([
            "bash", "-lc",
            "curl --proto '=https' --tlsv1.2 -sSf -L "
            "https://install.determinate.systems/nix | sh -s -- install"
        ])
    elif system == "Linux":
        run([
            "bash", "-lc",
            "curl -L https://nixos.org/nix/install | sh -s -- --daemon"
        ])
    else:
        sys.exit(f"Unsupported system for Nix bootstrap: {system}")


def nix_eval_attrnames(flake_ref: str, attr: str, *, cwd: Path) -> list[str]:
    """
    Return the attribute names of a flake output attrset.

    Example:
        nix eval --json .#nixosConfigurations \
          --apply 'x: builtins.attrNames x'
    """
    cmd = (
        f"nix eval --json {flake_ref}#{attr} "
        "--apply 'x: builtins.attrNames x'"
    )
    out = capture(bash_login(cmd), cwd=cwd)
    try:
        value = json.loads(out)
    except json.JSONDecodeError:
        raise RuntimeError(
            f"Failed to parse JSON from nix eval for {attr}:\n{out}"
        ) from None

    if not isinstance(value, list) or not all(isinstance(x, str) for x in value):
        raise RuntimeError(f"Unexpected nix eval result for {attr}: {value!r}")

    return sorted(value)


def discover_targets(*, cwd: Path) -> tuple[list[str], list[str]]:
    """
    Discover available NixOS hosts and Home Manager targets from the flake.

    Returns:
        (nixos_hosts, home_targets)
    """
    nixos_hosts = nix_eval_attrnames(".", "nixosConfigurations", cwd=cwd)
    home_targets = nix_eval_attrnames(".", "homeConfigurations", cwd=cwd)
    return nixos_hosts, home_targets


def default_mode_and_target(
    nixos_hosts: list[str],
    home_targets: list[str],
) -> tuple[str, str]:
    """
    Choose a sensible default mode and target for the current system.

    Preference order:
    - NixOS -> nixos + first matching host
    - macOS -> home + 'mac' if present
    - WSL   -> home + 'wsl' if present
    - Linux -> home + 'sam' if present
    """
    if have("nixos-rebuild") and nixos_hosts:
        if "homeStation" in nixos_hosts:
            return "nixos", "homeStation"
        return "nixos", nixos_hosts[0]

    system = platform.system()

    if system == "Darwin":
        if "mac" in home_targets:
            return "home", "mac"
        return "home", home_targets[0]

    if system == "Linux" and is_wsl():
        if "wsl" in home_targets:
            return "home", "wsl"
        return "home", home_targets[0]

    if "sam" in home_targets:
        return "home", "sam"

    return "home", home_targets[0]


def apply_home(target: str) -> None:
    """
    Apply a Home Manager configuration using the activationPackage from this flake.

    This avoids pinning a separate Home Manager version in the script; the version
    is taken from the flake inputs.
    """
    ensure_nix()

    # Build the activation package and get its store path
    expr = f"nix build --print-out-paths .#homeConfigurations.{target}.activationPackage"
    out = capture(bash_login(expr), cwd=REPO_ROOT).strip()
    if not out:
        raise RuntimeError("nix build returned no output path for activationPackage")

    # nix build may print multiple paths; activationPackage is one path, take the first line
    activation_path = out.splitlines()[0]
    activate = f"{activation_path}/activate"

    # Run activation
    run(bash_login(activate), cwd=REPO_ROOT)


def apply_nixos(host: str) -> None:
    """
    Apply a NixOS system configuration using nixos-rebuild.
    """
    cmd = f"sudo nixos-rebuild switch --flake .#{host}"
    run(bash_login(cmd), cwd=REPO_ROOT)


def main() -> None:
    """
    Command-line entry point.
    """
    parser = argparse.ArgumentParser(
        description="Bootstrap Nix if needed and apply NixOS/Home Manager from this flake."
    )
    parser.add_argument(
        "--repo",
        type=Path,
        default=REPO_ROOT,
        help="Path to flake repo (default: repo containing this script).",
    )
    parser.add_argument(
        "--list",
        action="store_true",
        help="List discovered targets and exit.",
    )

    sub = parser.add_subparsers(dest="mode")

    nixos = sub.add_parser("nixos", help="Apply a NixOS configuration")
    nixos.add_argument("host", nargs="?", help="nixosConfigurations.<host>")

    home = sub.add_parser("home", help="Apply a Home Manager configuration")
    home.add_argument("target", nargs="?", help="homeConfigurations.<name>")

    sub.add_parser("auto", help="Auto-select configuration (default)")

    args = parser.parse_args()

    global REPO_ROOT
    REPO_ROOT = args.repo.resolve()

    ensure_nix()

    try:
        nixos_hosts, home_targets = discover_targets(cwd=REPO_ROOT)
    except Exception as exc:
        print(f"Warning: failed to discover targets: {exc}", file=sys.stderr)
        nixos_hosts, home_targets = [], []

    if args.list:
        print("nixosConfigurations:")
        for h in nixos_hosts:
            print(f"  - {h}")
        print("homeConfigurations:")
        for t in home_targets:
            print(f"  - {t}")
        return

    mode = args.mode or "auto"

    if mode == "auto":
        mode, target = default_mode_and_target(nixos_hosts, home_targets)
    elif mode == "nixos":
        target = args.host or (nixos_hosts[0] if nixos_hosts else "")
    elif mode == "home":
        target = args.target or (home_targets[0] if home_targets else "")
    else:
        parser.error("Unknown mode")

    if not target:
        sys.exit("No valid target found or specified.")

    if mode == "nixos" and nixos_hosts and target not in nixos_hosts:
        sys.exit(f"Unknown NixOS host '{target}'. Choices: {', '.join(nixos_hosts)}")
    if mode == "home" and home_targets and target not in home_targets:
        sys.exit(f"Unknown home target '{target}'. Choices: {', '.join(home_targets)}")

    if mode == "nixos":
        apply_nixos(target)
    else:
        apply_home(target)


if __name__ == "__main__":
    main()
