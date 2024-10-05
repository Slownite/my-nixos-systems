{ config, lib, pkgs, ... }:

let
  polybarScript = ''
    export DISPLAY=:0
    export XAUTHORITY=${
      config.home.sessionVariables.XAUTHORITY or "$HOME/.Xauthority"
    }
    polybar top > /tmp/polybar.log 2>&1
  '';
in {
  services.polybar = {
    enable = true;

    # Ensure the service starts after the graphical session

    settings = {
      "bar/top" = {
        monitor = "HDMI-0";
        width = "100%";
        height = "30px";
        background = "#222";
        foreground = "#fff";
        modules-center = "date";
      };

      "module/date" = {
        type = "internal/date";
        interval = 60;
        time = "%H:%M";
        date = "%d.%m.%y";
        label = "%time%  %date%";
      };
      "module/i3" = {
        type = "internal/i3";
        pin-workspaces = true;
        show-urgent = true;
        strip-wsnumbers = true;
        index-sort = true;
        enable-click = false;
        enable-scroll = false;
        wrapping-scroll = false;
        reverse-scroll = false;
        fuzzy-match = true;

      };
    };

    script = polybarScript;
  };

  home.packages = [ pkgs.polybar ];
}
