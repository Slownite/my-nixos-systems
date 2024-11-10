{config, lib, pkgs, ...}:
{
  services.qemuGuest.enable = true; # Enable QEMU Guest for Proxmox
  boot.loader.grub.enable = true; # Use the boot drive for GRUB
  boot.loader.grub.devices = [ "nodev" ];
  boot.growPartition = true;
  nix.settings.trusted-users = [ "root" "@wheel" ]; # Allow remote updates
}
