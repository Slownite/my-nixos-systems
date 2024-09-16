{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;   
    };
    home.file = {
      "./doom.d" = {
        source = ./doom;
        target = "/home/sam/.doom.d";
        recursive = true;
      };
    };
}

