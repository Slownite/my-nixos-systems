{ config, lib, pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    shellIntegration.enableZshIntegration = true;

  };
  home.file = {
    "./kitty" = {
        source = ./kitty;
        target = ".config/kitty";
        recursive = true;
    };
  };
}
