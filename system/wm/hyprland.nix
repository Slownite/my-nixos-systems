{pkgs, ... }:

{

  programs.hyprland.enable = true;
  programs.hyprland.xwayland.enable = true;
  hardware = {
    opengl.enable = true;
    nvidia.modesetting.enable = true;
 };
}
