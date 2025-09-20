{ config, lib, pkgs, home-manager, ... }:

{
  imports =
    [ ./zsh.nix ./tmux.nix ./starship.nix ./fish.nix ./fastfetch.nix ./claude_code.nix ];
  home.packages = with pkgs; [ bat wget curl fzf ];
}
