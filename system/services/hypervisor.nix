{ config, lib, pkgs, ... }:
let cfg = config.hypervisor;
in {
  options.hypervisor = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Enable or disable the hypervisor module
      '';
    };
    hostIpAdress = lib.mkOption {
      type = lib.types.str;
      default = "192.168.1.100";
      description = "choose the ip adress of the host";
    };
  };
  config = lib.mkIf cfg.enable {
    # Load KVM kernel modules
    boot.kernelModules = [ "kvm" "kvm-intel" "kvm-amd" ];
    # Enable libvirtd service
    virtualisation.libvirtd = {
      enable = true;
      qemu.package = pkgs.qemu_kvm; # Use KVM-enabled QEMU
    };

    environment.systemPackages = with pkgs; [ virt-manager ];
    # Add your user to the 'libvirtd' group
    users.users.sam.extraGroups = [ "libvirtd" ];

    networking = {
      # Disable DHCP on the physical interface
      interfaces.enp3s0.useDHCP = false;

      # Create a bridge 'br0' including your physical interface
      bridges.br0.interfaces = [ "enp3s0" ];

      # Assign IP to the bridge
      interfaces.br0.ipv4.addresses = [{
        address = cfg.hostIpAdress; # Your host's IP
        prefixLength = 24;
      }];

      defaultGateway = "192.168.8.1"; # Your network's gateway
      nameservers = [ "8.8.8.8" "8.8.4.4" ];

      firewall = {
        enable = true;
        allowedTCPPorts = [ 22 ];
        allowedUDPPorts = [ ];
      };
    };
    # Enable the OpenSSH daemon.
    services.openssh = lib.mkDefault {
      enable = true;
      settings = {
        PermitRootLogin = "no"; # Recommended for security
        PasswordAuthentication = false; # Enforce key-based auth
      };
    };
  };
}
