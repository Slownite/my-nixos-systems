{ config, lib, pkgs, ... }:

{
  services.voxy = {
    enable = true;
    modelSize = "small";
  };
  users.users.sam.extraGroups = [ "input" ];
}
