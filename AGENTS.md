# Agent Instructions for NixOS Systems

This repository contains declarative NixOS and Home Manager configurations using Nix Flakes. 
When asked to modify this repository, refer to the guidelines below and utilize the workflows and skills in the `.agents/` directory.

## Repository Layout
- `hosts/`: Host-specific NixOS configurations (e.g., hardware configuration, bootloader).
- `system/`: Global NixOS configurations, system packages, and services that don't depend on the user.
- `user/`: Home Manager modules and dotfiles, configuring user-level applications, window managers (i3wm), terminals, and theming.
- `flake.nix`: The central entry point defining inputs and outputs (NixOS configurations and Home Manager configurations).

## Agent Workflows
Workflows and skills are located in the `.agents/` directory:
- **Workflows (`.agents/workflows/`)**: Automated guides on testing and deploying NixOS or Home Manager configurations.
- **Skills (`.agents/skills/`)**: Expertise context defining architectural boundaries inside this configuration.

## Core Commands
Whenever modifying system-level configurations (`hosts/`, `system/`):
```bash
sudo nixos-rebuild switch --flake ./#<device_name>
```

Whenever modifying user-level configurations (`user/`):
```bash
home-manager switch --flake ./#sam
```
