{ config, lib, pkgs, ... }:

{
  # Load KVM kernel modules
  boot.kernelModules = [ "kvm" "kvm-intel" "kvm-amd" ];
  services.cockpit = {
    enable = true;
    packages = with pkgs;
      [ cockpit-machines ]; # Include the virtualization module
    # Optionally, restrict listening interfaces
    # listen = [ "0.0.0.0" ];  # Listen on all interfaces (use with caution)
  };
  # Enable libvirtd service
  services.libvirtd = {
    enable = true;
    qemuPackage = pkgs.qemu_kvm; # Use KVM-enabled QEMU
    virtlockd.enable = true;
    virtlogd.enable = true;
  };

  # Add your user to the 'libvirtd' group
  users.users.sam.extraGroups = [ "libvirtd" ];

  # Optional: Install VM management tools
  environment.systemPackages = with pkgs; [ virt-manager virt-viewer ];

  networking = {
    # Disable DHCP on the physical interface
    interfaces.enp3s0.useDHCP = false;

    # Create a bridge 'br0' including your physical interface
    bridges.br0.interfaces = [ "enp3s0" ];

    # Assign IP to the bridge
    interfaces.br0.ipv4.addresses = [{
      address = "192.168.1.100"; # Your host's IP
      prefixLength = 24;
    }];

    defaultGateway = "192.168.8.1"; # Your network's gateway
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
  };
}
