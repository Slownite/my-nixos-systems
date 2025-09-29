{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    ripgrep
    fd
    shellcheck
    direnv
    nixfmt-classic
    nixd
    ispell
    dockfmt
    proselint
    poetry
    shfmt
  ];
}

