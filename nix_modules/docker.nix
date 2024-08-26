{ config, lib, pkgs, ... }:

{
  options = {
    docker = lib.mkEnableOption "enables docker";
  };
  config = lib.mkIf config.docker.enable {

    environment.systemPackages = with pkgs; [
      docker
      docker-compose
    ];
    virtualisation.docker.enableOnBoot = true;
    virtualisation.docker.enable = true;
  };
}
