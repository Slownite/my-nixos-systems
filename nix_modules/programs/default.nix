{ config, lib, pkgs, ... }:

{
  imports = [
    ./steam.nix
    ./firefox.nix
    ./kitty.nix
    ./git.nix
    ./nvim.nix
    ./emacs.nix
    ./zsh.nix
  ];
}
