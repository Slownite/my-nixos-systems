{ config, lib, pkgs, ... }:
let storage = "/nextcloud-data/";
in {
  services.nextcloud = {
    enable = true;
    datadir = "${storage}";
    hostName = "nextcloud";
    config = { adminpassFile = "/home/sam/netcloud_password"; };
  };
}
