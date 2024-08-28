
{ config, lib, pkgs, ... }:

{

  options = {
      gaming.enable = lib.mkEnableOption "enable gaming program";
  };
  config = lib.mkIf config.gaming.enable {
    programs = {
      steam = {
        enable = true;
        gamescopeSession.enable = true;
        gamemode.enable = true;
        extraCompatPackages = {

          environment.systemPackages = with pkgs; [
            proton-ge-bin
            mangohud
          ];
        };
      };
    };
    users.users.sam.packages = with pkgs; [
      heroic
      bottles
    ];
  };
}
