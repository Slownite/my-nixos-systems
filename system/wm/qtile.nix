{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    dmenu
  ];
  qtile = {
    enable = true;
    configFile = ./config.py;
  };

}
