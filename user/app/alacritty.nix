{ config, lib, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = { shell = { program = "/home/sam/.nix-profile/bin/nu"; }; };
  };
}
