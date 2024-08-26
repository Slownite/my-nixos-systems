{ config, lib, pkgs, ... }:

{

  options = {
      steam.enable = lib.mkEnableOption "enable steam";
  };
  config = lib.mkIf config.steam.enable {
    programs = {
      steam = {
        enable = true;
        extraCompatPackages = {

          environment.systemPackages = with pkgs; [
            proton-ge-bin
          ];
        };

      };
    };
  };
}
