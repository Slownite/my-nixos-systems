{ config, lib, pkgs, ... }:

{
  options = {
      experimental.enable = lib.mkEnableOption "enable experimental feature";
  };
  config = lib.mkIf config.experimental.enable {
    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
  };
}
