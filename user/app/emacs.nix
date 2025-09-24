{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    ripgrep
    fd
    zig
    shellcheck
    direnv
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
  

  # programs.doom-emacs = {
  #   enable = true;
  #   doomDir = ./doom; # or e.g. `./doom.d` for a local configuration
  # };
  # home.file = {
  #   "./doom.d" = {
  #     source = ./doom;
  #     target = "/home/sam/.doom.d";
  #     recursive = true;
  #   };
}

