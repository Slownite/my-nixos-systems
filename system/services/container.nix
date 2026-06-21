{ config, lib, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
      docker_29
      docker-compose
    ];
    virtualisation.docker.enableOnBoot = true;
    virtualisation.docker.enable = true;
    virtualisation.docker.package = pkgs.docker_29;
}
