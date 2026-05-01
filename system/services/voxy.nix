{ config, lib, pkgs, ... }:

{
  services.voxy.enable = true;
  users.users.sam.extraGroups = [ "input" ];
}
