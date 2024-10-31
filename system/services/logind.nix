{ config, lib, pkgs, ... }:

{
  services.logind = {
    # Ignore the lid switch action to keep the server running.
    lidSwitch = "lock";
    lidSwitchDocked = "lock";
    lidSwitchExternalPower = "lock";
  };
}
