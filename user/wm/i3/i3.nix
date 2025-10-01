{ config, lib, pkgs, ... }:
let cfg = config.xsession.windowManager.i3;
in {
  imports = [
    ./polybar.nix
    ./picom.nix
    ./dunst.nix
    ../../app/flameshot.nix
 #   ./wallpaper.nix
  ];
  programs.rofi = {
    enable = true;
    theme = "${../../theme/rofi-themes/squared-nord.rasi}";
  };
  programs.feh.enable = true;
  xsession.windowManager.i3 = {
    enable = true;
    extraConfig = ''
      exec --no-startup-id feh --bg-scale ${../../theme/nord.png}
      exec --no-startup-id polybar main > /tmp/polybar.log 2>&1
      exec --no-startup-id nm-applet
      exec --no-startup-id lxqt-policykit-agent
    '';

    # exec --no-startup-id ${config.home.file."wallpaper.sh"}/bin/wallpaper.sh
    # exec --no-startup-id alacritty && i3-msg "[class="Alacritty"] move scratchpad"
    package = pkgs.i3;
    config = {
      bars = [ ];
      modifier = "Mod4";
      terminal = "alacritty";
      menu = "rofi -show run";
      gaps = {
        inner = 30;
        outer = 5;
      };
      keybindings = {
        "${cfg.config.modifier}+Return" = "exec ${cfg.config.terminal}";
        "${cfg.config.modifier}+Shift+q" = "kill";
        "${cfg.config.modifier}+space" = "exec ${cfg.config.menu}";
        "${cfg.config.modifier}+b" = "exec floorp";
        "${cfg.config.modifier}+Shift+b" = "exec brave";
        "${cfg.config.modifier}+e" = "exec nautilus";
        "${cfg.config.modifier}+bracketright" = "exec flameshot gui";
        "${cfg.config.modifier}+c" = "exec emacsclient -c -a 'emacs'";
        "${cfg.config.modifier}+Shift+x" =
          "exec i3lock -i ${../../theme/nord.png}";

        "${cfg.config.modifier}+h" = "focus left";
        "${cfg.config.modifier}+j" = "focus down";
        "${cfg.config.modifier}+k" = "focus up";
        "${cfg.config.modifier}+l" = "focus right";

        "${cfg.config.modifier}+Shift+h" = "move left";
        "${cfg.config.modifier}+Shift+j" = "move down";
        "${cfg.config.modifier}+Shift+k" = "move up";
        "${cfg.config.modifier}+Shift+l" = "move right";

        "${cfg.config.modifier}+z" = "split h";
        "${cfg.config.modifier}+w" = "split v";
        "${cfg.config.modifier}+f" = "fullscreen toggle";

        "${cfg.config.modifier}+s" = "layout toggle split";
        "${cfg.config.modifier}+Shift+z" = "layout tabbed";

        "${cfg.config.modifier}+Shift+d" = "floating toggle";
        "${cfg.config.modifier}+d" = "focus mode_toggle";

        "${cfg.config.modifier}+a" = "focus parent";

        "${cfg.config.modifier}+Shift+semicolon" = "move scratchpad";
        "${cfg.config.modifier}+semicolon" = "scratchpad show";

        "${cfg.config.modifier}+1" = "workspace number 1";
        "${cfg.config.modifier}+2" = "workspace number 2";
        "${cfg.config.modifier}+3" = "workspace number 3";
        "${cfg.config.modifier}+4" = "workspace number 4";
        "${cfg.config.modifier}+5" = "workspace number 5";
        "${cfg.config.modifier}+6" = "workspace number 6";
        "${cfg.config.modifier}+7" = "workspace number 7";
        "${cfg.config.modifier}+8" = "workspace number 8";
        "${cfg.config.modifier}+9" = "workspace number 9";
        "${cfg.config.modifier}+0" = "workspace number 10";

        "${cfg.config.modifier}+Shift+1" =
          "move container to workspace number 1";
        "${cfg.config.modifier}+Shift+2" =
          "move container to workspace number 2";
        "${cfg.config.modifier}+Shift+3" =
          "move container to workspace number 3";
        "${cfg.config.modifier}+Shift+4" =
          "move container to workspace number 4";
        "${cfg.config.modifier}+Shift+5" =
          "move container to workspace number 5";
        "${cfg.config.modifier}+Shift+6" =
          "move container to workspace number 6";
        "${cfg.config.modifier}+Shift+7" =
          "move container to workspace number 7";
        "${cfg.config.modifier}+Shift+8" =
          "move container to workspace number 8";
        "${cfg.config.modifier}+Shift+9" =
          "move container to workspace number 9";
        "${cfg.config.modifier}+Shift+0" =
          "move container to workspace number 10";

        "${cfg.config.modifier}+Shift+c" = "reload";
        "${cfg.config.modifier}+Shift+r" = "restart";
        "${cfg.config.modifier}+Shift+e" =
          "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";

        "${cfg.config.modifier}+r" = "mode resize";
      };
    };
  };
}
