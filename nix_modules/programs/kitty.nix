{ config, lib, pkgs, ... }:

{
  programs.kitty = {
    enable = true;
  };
  home.file = {
    "kitty.conf".source = "./kitty/kitty.conf";
  };
}
