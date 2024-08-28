{ config, lib, pkgs, ... }:

{

  options = {
      steam.enable = lib.mkEnableOption "enable steam";
  };
  config = lib.mkIf config.steam.enable {
  };
}
