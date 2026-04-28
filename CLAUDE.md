# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

**Apply system-level changes** (modifications to `hosts/` or `system/`):
```bash
sudo nixos-rebuild switch --flake ./#homeStation
```

**Apply user-level changes** (modifications to `user/`):
```bash
home-manager switch --flake ./#sam
```

**Check Nix syntax** without applying:
```bash
nix flake check
```

**Format Nix files**:
```bash
nixfmt <file.nix>
# or
alejandra <file.nix>
```

## Architecture

This is a flake-based, multi-host NixOS + Home Manager configuration. The key architectural rule is **separation of concerns**:

| Layer | Directory | What goes here |
|-------|-----------|----------------|
| Host-specific | `hosts/<name>/` | Bootloader, hardware tweaks, hardware-configuration.nix, the top-level home.nix that imports user modules |
| System-wide | `system/` | Services, hardware drivers, system packages (anything not user-specific) |
| User-space | `user/` | Home Manager modules: dotfiles, WM themes, user packages, shell config |

### Flake outputs

- `nixosConfigurations.homeStation` — full NixOS system (imports `home-manager` module + `hosts/home_station/configuration.nix`)
- `homeConfigurations.sam` — standalone Home Manager for the Linux desktop
- `homeConfigurations.mac` — Home Manager for macOS (x86_64-darwin, no WM config)
- `homeConfigurations.wsl` — minimal Home Manager for WSL

`unstablePkgs` (from `nixos-unstable`) is passed as `extraSpecialArgs` and available in Home Manager modules alongside `pkgs` (from `nixos-25.11`). Use `unstablePkgs.<package>` when you need a newer version than stable provides.

### Theming

All configurations use **Stylix** for unified theming (Nord color scheme via `user/theme/`, DejaVu fonts, Bibata cursor). Stylix is injected as a flake input and applied as a module in every `homeConfiguration`. Per-app style overrides live in `user/theme/`.

### Adding a new user module

1. Create the module file under the appropriate `user/` subdirectory.
2. Import it in the relevant `hosts/<name>/home.nix`.
3. Run `home-manager switch --flake ./#sam` to apply.

### Adding a new system service or hardware module

1. Create the module file under `system/services/` or `system/hardware/`.
2. Import it in `hosts/home_station/configuration.nix`.
3. Run `sudo nixos-rebuild switch --flake ./#homeStation` to apply.
