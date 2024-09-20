{config, lib, pkgs, ...}:
{

  programs.waybar = {
    enable = true;
    settings = [
      {
        "layer"= "top";
        "height" = 24;
        "modules-left"= ["sway/workspaces" "sway/mode"];
        "modules-center"= ["sway/window"];
        "modules-right"= ["pulseaudio" "network" "cpu" "memory" "tray" "clock"];
        "sway/window"= {
            
        };
        "clock"= {
            "format-alt"= "{:%a, %d. %b  %H:%M}";
        };
      }
    ];
  };
}
