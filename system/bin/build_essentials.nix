{ config, lib, pkgs, ... }:
{
    programs.nix-ld.enable = true;
    programs.nix-ld.libraries = with pkgs; [
      stdenv.cc.cc.lib
    ];

    environment.systemPackages = with pkgs; [
      bison
      flex
      fontforge
      makeWrapper
      pkg-config
      gnumake
      gcc
      libiconv
      autoconf
      automake
      libtool
      cmake
    ];
}
