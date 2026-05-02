default:
    @just --list

# Bootstrap a machine from scratch
install machine:
    @case "{{machine}}" in \
        nixos) \
            nix profile install nixpkgs#home-manager && \
            sudo nixos-rebuild switch --flake ./#homeStation && \
            home-manager switch --flake ./#sam ;; \
        macbook) \
            curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install && \
            nix run home-manager/release-25.11 -- switch --flake ./#mac ;; \
        wsl) \
            curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install && \
            nix run home-manager/release-25.11 -- switch --flake ./#wsl ;; \
        *) echo "Unknown machine: {{machine}}. Use nixos, macbook, or wsl." && exit 1 ;; \
    esac

# Apply latest config changes
switch machine:
    @case "{{machine}}" in \
        nixos) \
            sudo nixos-rebuild switch --flake ./#homeStation && \
            home-manager switch --flake ./#sam ;; \
        macbook) \
            home-manager switch --flake ./#mac ;; \
        wsl) \
            home-manager switch --flake ./#wsl ;; \
        *) echo "Unknown machine: {{machine}}. Use nixos, macbook, or wsl." && exit 1 ;; \
    esac
