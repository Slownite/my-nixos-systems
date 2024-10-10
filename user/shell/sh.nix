{ config, lib, pkgs, home-manager, ... }:

{
  imports =
    [ ./nushell.nix ./tmux.nix ./starship.nix ./fish.nix ./fastfetch.nix ];
  home.packages = with pkgs; [ bat wget curl fzf ];
}
