{ config, lib, pkgs, ... }:

{
  services.ollama = {
    package = pkgs.ollama;
    enable = true;
    acceleration = "cuda";
  };
  services.open-webui = {
    enable = true;
    package = pkgs.open-webui;
  };
}
