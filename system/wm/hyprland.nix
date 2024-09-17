{pkgs, ... }:

{

  programs.hyprland.enable = true;
  hardware = {
    opengl.enable = true;
    nvidia.modesetting.enable = true;
 };
  xdg.portal.enable = true;

  xdg.portal.config.common.default = "*";

  environment.defaultPackages = with pkgs; [
    wl-clipboard
    mako
    waybar
    rofi-wayland
    libnotify
    swww
  ];
}