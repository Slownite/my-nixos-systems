{ config, lib, pkgs, home-manager, ... }:

{
  imports = [
    ./nushell.nix
    ./tmux.nix
    ./starship.nix
  ];
  home.packages = with pkgs; [
      bat
      wget
      curl
      fastfetch
      fzf
  ];
}
