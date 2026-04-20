---
name: NixOS Configuration Manager
description: Guidelines on how to update and maintain the NixOS modules in this repository.
---

# NixOS Config Skill

When modifying this repository, adhere to the following rules:

1. **Separation of Concerns**:
   - **System-wide configs**: Services, hardware drivers, and system packages go in `system/`.
   - **User-specific configs**: Dotfiles, window manager themes, and user packages go in `user/`.
   - **Host-specific configs**: Bootloader and specific hardware tweaks go in `hosts/`.

2. **Nix Syntax**:
   - Ensure syntactically sound `.nix` files.
   - You can use formatting tools if needed.

3. **Applying Changes**:
   - Since this is a Flake-based system, changing configurations usually requires you to run the relevant `nixos-rebuild switch` or `home-manager switch` command. Check the `.agents/workflows/` directory for specifics.
