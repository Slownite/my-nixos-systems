{ config, lib, pkgs, ... }:
let colors = config.lib.stylix.colors;
in {
  services.dunst = {
    enable = true;
    settings = {
      # Customize Dunst settings as needed
      global = {
        format = "<b>%s</b>"; # Bold text in notifications
        geometry = "300x5-10+30";
        icon_position = "left";
        frame_width = 2;
        origin = "top-right";
      };
      # Urgency levels
      urgency_low = { timeout = 5; };

      urgency_normal = { timeout = 10; };

      urgency_critical = { timeout = 0; };
    };
  };
}
