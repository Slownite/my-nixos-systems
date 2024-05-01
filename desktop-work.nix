# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    /etc/nixos/hardware-configuration.nix
    ./common.nix
    ./work/common-work.nix
  ];

  boot.initrd.luks.devices."luks-fbdacde1-76e0-4d9c-89cc-e0cab5e7d2d4".device = "/dev/disk/by-uuid/fbdacde1-76e0-4d9c-89cc-e0cab5e7d2d4";
  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  networking.hostName = "work"; # Define your hostname.
  networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.
}
