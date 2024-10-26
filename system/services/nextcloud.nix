{ config, lib, pkgs, ... }:
let storage = "/nextcloud-data/";
in {
  services.nextcloud = {
    enable = true;
    dataDir = "${storage}";
  };
}
