{ config, lib, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
  };
  #home.file = {
   # "./nvim" = {
    #  source = ./nvim;
     # target = ".config/nvim";
      #recursive = true;
    #};
  #};
  home.packages = with pkgs; [spacevim];
}
