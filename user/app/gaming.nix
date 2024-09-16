
{ config, lib, pkgs, ... }:

{

    programs = {
      steam = {
        enable = true;
        gamescopeSession.enable = true;
        extraPackages = with pkgs; [
		gamescope
	];
        # extraCompatPackages = with pkgs; [
        #     proton-ge-bin
        #   ];
      };
    };
    users.users.sam.packages = with pkgs; [
      heroic
      bottles
      mangohud
    ];
}
