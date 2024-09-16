{ config, lib, pkgs, ... }:

{
  programs.rofi = {
    enable = true;
  };
  home.file = {
  "rofi" = {
      source = ./rofi;
      target = ".config/rofi";
      recursive = true;
    };
  };
}
