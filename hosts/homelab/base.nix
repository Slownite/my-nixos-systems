{config, lib, pkgs, ...}: 
let
  cfg = config.base;
in
{
  imports = [
    ./common.nix
    ../../system/services/qmeu.nix
  ];
  options.base = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "enable the base server";
    };
    name = lib.mkOption {
    type = lib.types.str;
    description = "name of the machine";
    };
  };
  config = lib.mkIf cfg.enable {
    programs.bash = {
      shellInit = ''
      if [[ $- == *i* ]]; then
        figlet ${cfg.name}
      fi
      '';
    };
    networking = {
      hostName = cfg.name;
      firewall = {
        enable = true;
        allowedTCPPorts = [ 22 111 2049];
        allowedUDPPorts = [ 111 ];
      };
    };
  };
}
