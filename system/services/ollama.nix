{ config, lib, pkgs, ... }:

{
  services.ollama = {
    package = pkgs.ollama-cuda;
    enable = true;
  };
 # services.open-webui = {
 #  enable = true;
 #  package = pkgs.open-webui;
 #};
}
