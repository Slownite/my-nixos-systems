{ config, lib, pkgs, ... }: {

  services.xserver = {
    enable = true;
    windowManager = {
      i3 = {
        enable = true;
        extraPackages = with pkgs; [
          i3status # gives you the default i3 status bar
          i3lock # default i3 screen locker
        ];
      };
    };
  };
  services.displayManager = { defaultSession = "none+i3"; };
}
