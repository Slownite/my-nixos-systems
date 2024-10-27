#!/usr/bin/env sh

# Exit the script if any command fails
set -e

# General function to handle building system for any machine
build_system() {
    machine=$1
    config_flake=$2

    echo "Delete current NixOS configuration..."
    sudo rm -f /etc/nixos/configuration.nix

    echo "Building system for $machine..."
    sudo nixos-rebuild switch --flake "./#$config_flake" --impure

    echo "Build system done, $machine is online."
}

# Display help message
show_help() {
    echo "Usage: $0 [machine]"
    echo ""
    echo "Available machines:"
    echo "  hades    - Build and switch to the Hades configuration."
    echo "  hermes   - Build and switch to the Hermes configuration."
    echo "  homeStation   - Build and switch to the homeStation configuration."
    echo ""
}

# Entry point
machine_configuration=$1

case $machine_configuration in
"hades")
    build_system "Hades" "homelabHades"
    ;;
"hermes")
    build_system "Hermes" "homelabHermes"
    ;;
"homeStation")
    build_system "Hermes" "homeStation"
    ;;
*)
    show_help
    ;;
esac
