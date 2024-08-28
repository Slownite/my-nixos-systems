{ config, lib, pkgs, ... }:

{
  imports = [
    ./firefox.nix
    ./kitty.nix
    ./git.nix
    ./nvim.nix
    ./emacs.nix
    ./zsh.nix
    ./nushell.nix
    ./qtile.nix
  ];
}
