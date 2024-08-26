{ config, lib, pkgs, ... }:

{
  programs.doom-emacs = {
    enable = true;
    emacsPackage = pkgs.emacs;
    doomPrivateDir = "./.doom.d";
  };
}
