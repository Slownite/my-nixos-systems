{ config, lib, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
  };
  home.file = {
    "./nvim" = {
      source = ./nvim;
      recursive = true;
    };
  };
}
