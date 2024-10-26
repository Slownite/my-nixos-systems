{ config, lib, pkgs-unstable, ... }:

{
  services.ollama = {
    package = pkgs-unstable.ollama;
    enable = true;
    acceleration = "cuda";
  };
  services.open-webui = {
    enable = true;
    package = pkgs-unstable.open-webui;
  };
}
