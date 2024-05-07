# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    /etc/nixos/hardware-configuration.nix
    ./common.nix
    ./homelab/common-homelab.nix
  ];

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
  networking.hostName = "mark";

  # Configure keymap in X11
  services.xserver = {
    layout = "fr";
    xkbVariant = "";
  };

  # Configure console keymap
  console.keyMap = "fr";

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  users.motd = ''
       /\     /\
    {  `---'  }
    {  O   O  }
    ~>  V  <~~
     \  \|/  /
      `-----'____
      /     \    \_
     {       }\  )_\_   _
     |  \_/  |/ /  \_\_/ )
      \__/  /(_/     \__/
        (__/

  '';
  services.dnsmasq.settings = {
    enable = true; # Enable dnsmasq service
    server = [
      "127.0.0.1"
      "192.168.8.190"
    ] # Specify IP addresses for dnsmasq to listen on
    ;
  };

  fileSystems."/mnt/bdrive" = {
    device = "/dev/disk/by-uuid/09d5ea97-bc03-43bb-a07c-258aa0b170ff"; # Use the UUID or device path
    fsType = "btrfs"; # Use the appropriate filesystem type
  };
  # services.nfs.server = {
  #   enable = true;
  #   exports = ''
  #     /mnt/bdrive/data *(rw,sync,no_subtree_check,no_root_squash)
  #   '';
  # };
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [
    22
    9443 # portainer
    8096 # jellyfin
    443 # nextcloud
    53 # pihole
    8070 # pihole
  ];
  networking.firewall.allowedUDPPorts = [
    53 # pihole
  ];
}
