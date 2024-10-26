{ config, lib, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
      docker
      docker-compose
    ];
    virtualisation.docker.enableOnBoot = true;
    virtualisation.docker.enable = true;
}
