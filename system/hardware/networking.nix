{pkgs, lib, config, ...}:
{

  # Enable network
  networking.networkmanager.enable = true;
  programs.nm-applet.enable = true;
  environment.systemPackages = with pkgs; [ polkit_gnome ];
}
