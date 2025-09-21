{ config, lib, pkgs, ... }:

let
  colors = config.lib.stylix.colors;
  i3SupportPolybar = pkgs.polybar.override {
    i3Support = true;
    alsaSupport = true;
    iwSupport = true;
    githubSupport = true;
  };
in {
  services.polybar = {
    enable = true;
    package = i3SupportPolybar;

    settings = {
      "bar/main" = {
        monitor = "HDMI-0";
        width = "100%";
        height = "30px";
        background = "#${colors.base00}";
        foreground = "#${colors.base05}";
        border-left-size = 1;
        border-left-color = "#${colors.base00}";
        border-right-size = 1;
        border-right-color = "#${colors.base00}";
        border-top-size = 2;
        border-top-color = "#${colors.base00}";
        border-bottom-size = 2;
        border-bottom-color = "#${colors.base00}";
        font-0 = "FantasqueSansMNerdFont:size=12:weight=bold:style=Bold;1";
        font-1 = "Kochi Gothic:style=bold:weight=bold:size=12;1";

        modules-center = "time";
        # Removed "tray" here — the systray is NOT a module in polybar
        modules-right = "cpu icon-cpu spacer date";
        modules-left = "i3";

        # ✅ Real systray settings (this is what docks nm-applet)
        tray-position = "right";   # left/center/right
        tray-padding = 2;
        tray-maxsize = 20;
        # tray-scale = 1.0        # uncomment if you need size adjustments
      };

      "module/spacer" = {
        type = "custom/text";
        format = "   ";
      };
      "module/icon-cpu" = {
        type = "custom/text";
        format = " ";
      };
      "module/pipewire-volume" = {
        type = "custom/script";
        exec = "wpctl get-volume @DEFAULT_AUDIO_SINK@";
        interval = 1;
        label-muted = "🔇 muted";
        label-volume = "%output%";
        format-volume = "<ramp-volume> <label-volume>";
        ramp-volume-0 = "🔈";
        ramp-volume-1 = "🔉";
        ramp-volume-2 = "🔊";
        click-right = "pavucontrol";
      };
      "module/time" = {
        type = "internal/date";
        interval = 60;
        time = "%H:%M";
        date = "%d.%m.%y";
        label = "%time%";
      };
      "module/network" = {
        type = "internal/network";
        interval = 3.0;
        accumulate-stats = true;
        speed-unit = "";
      };
      "module/date" = {
        type = "internal/date";
        interval = 3600;
        date = "%d.%m.%y";
        label = "%date%";
      };
      "module/i3" = {
        type = "internal/i3";
        label-unfocused-foreground = "#${colors.base03}";
        label-urgent-foreground = "#${colors.base09}";
        label-unfocused-padding = 1;
        label-focused-padding = 1;
        label-urgent-padding = 1;
      };
      "module/cpu" = {
        type = "internal/cpu";
        interval = 0.5;
        warn-percentage = 95;
      };

      # ❌ Removed "module/tray" (polybar systray is configured in bar/main)
    };

    script = "";
  };
}
