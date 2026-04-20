---
description: Apply Home Manager Configuration
---

# Apply Home Manager Configuration

This workflow applies the user-space configuration using Home Manager directly from the local flake.

1. This workflow should be used when configurations inside the `user/` directory are modified.
2. Run the home-manager rebuild command for the primary user `sam`:
```bash
home-manager switch --flake ./#sam
```
