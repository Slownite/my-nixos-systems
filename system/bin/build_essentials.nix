{ config, lib, pkgs, ... }:
{

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
