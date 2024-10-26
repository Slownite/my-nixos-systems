{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    ripgrep
    fd
    zig
    shellcheck
    direnv
    nerdfonts
    nixfmt-classic
    nixd
    nodejs_22
    ispell
    dockfmt
    proselint
    llvmPackages_12.clang-tools
    poetry
    glslang
    shfmt
    zls
  ];
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
  home.file = {
    "./doom.d" = {
      source = ./doom;
      target = "/home/sam/.doom.d";
      recursive = true;
    };
  };
}

