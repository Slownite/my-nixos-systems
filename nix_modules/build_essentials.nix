{ config, lib, pkgs, ... }:

{
  options = {
    build_essentials = lib.mkEnableOption "enables build_essentials";
  };
  config = lib.mkIf config.build_essentials.enable {

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
      home-manager #think where to put this later
    ];
  };
}
