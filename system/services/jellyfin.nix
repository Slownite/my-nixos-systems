{ config, lib, pkgs, ... }:
let dataDir = /data;
in {
  services.jellyfin = {
    enable = true;
    openFirewall = true;
    dataDir = "${dataDir}";
    user = "sam";
  };
}
