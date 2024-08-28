{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    dmenu
  ];
  programs.rofi = {
    enable = true;
  };
  home.file.qtile_config = {
  source = ./qtile;
  target = ".config/qtile";
  recursive = true;
  };
  home.file.rofi = {
    source = ./rofi;
    target = ".config/rofi";
    recursive = true;
  };
}
