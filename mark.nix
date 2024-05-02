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
  services.dnsmasq = {
    enable = true; # Enable dnsmasq service
    extraConfig = ''
      listen-address=127.0.0.1,192.168.1.2 # Specify IP addresses for dnsmasq to listen on
    '';
  };
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [
    22
    9443 # portainer
    8096 # jellyfin
    53 # pi-hole
  ];
  networking.firewall.allowedUDPPorts = [
    53 # pi-hole
    67 # Only required if you are using Pi-hole as your DHCP server
  ];
}
