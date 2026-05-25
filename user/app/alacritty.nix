{ config, lib, pkgs, unstablePkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      terminal.shell = {
        program = "${pkgs.xonsh}/bin/xonsh";
      };
    };
  };
}
