default:
    @just --list

_check-machine machine:
    @case "{{machine}}" in \
        nixos|macbook|wsl) ;; \
        *) echo "Unknown machine: {{machine}}. Use nixos, macbook, or wsl." >&2; exit 1 ;; \
    esac

# Bootstrap a machine from scratch
install machine: (_check-machine machine)
    @just _install-{{machine}}

_install-nixos:
    nix profile install nixpkgs#home-manager
    sudo nixos-rebuild switch --flake ./#homeStation
    home-manager switch --flake ./#sam

_install-macbook:
    curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
    nix run home-manager/release-25.11 -- switch --flake ./#mac

_install-wsl:
    curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
    nix run home-manager/release-25.11 -- switch --flake ./#wsl

# Apply latest config changes
switch machine: (_check-machine machine)
    @just _switch-{{machine}}

_switch-nixos:
    sudo nixos-rebuild switch --flake ./#homeStation
    home-manager switch --flake ./#sam

_switch-macbook:
    home-manager switch --flake ./#mac

_switch-wsl:
    home-manager switch --flake ./#wsl

# Apply user-level (home-manager) changes only
home machine: (_check-machine machine)
    @just _home-{{machine}}

_home-nixos:
    home-manager switch --flake ./#sam

_home-macbook:
    home-manager switch --flake ./#mac

_home-wsl:
    home-manager switch --flake ./#wsl

# Check the flake for syntax and evaluation errors
check:
    nix flake check

# Update all flake inputs to their latest revisions
update:
    nix flake update

# Format every Nix file in the tree with alejandra
fmt:
    alejandra .

# Roll back to the previous NixOS generation
rollback:
    sudo nixos-rebuild switch --rollback

# Garbage collect old generations (system + user)
gc:
    sudo nix-collect-garbage -d
    nix-collect-garbage -d

# Open a nix repl preloaded with this flake
repl:
    nix repl .
