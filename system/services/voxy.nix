{ config, lib, pkgs, ... }:

{
  services.voxy = {
    enable = true;
    modelSize = "tiny";
  };
  users.users.sam.extraGroups = [ "input" ];
}
