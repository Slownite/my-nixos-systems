---
description: Apply NixOS System Configuration
---

# Apply System Configuration

This workflow rebuilds the NixOS system configuration using the local flake.

1. Verify which host configuration you currently want to apply. You can look in the `hosts/` directory to see the available device names.
2. Run the system rebuild command using the selected host:
```bash
sudo nixos-rebuild switch --flake ./#<device_name>
```
