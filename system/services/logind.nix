{ config, lib, pkgs, ... }:

{
  services.logind = {
    lidSwitch = "ignore";
    lidSwitchDocked = "ignore";
  };
}
