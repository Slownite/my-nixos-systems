
{ config, lib, pkgs, ... }:

{
  wayland.windowManager.hyprland = {
    enable = true;
  # Optional, hint Electron apps to use Wayland:
    systemd.variables = ["--all"];
    settings = {
      monitor="HDMI-A-1,preferred,auto,1";
      
      "$terminal" = "alacritty";
      "$fileManager" = "nautilus";
      "$menu" = "rofi --show run";
      "$browser" = "firefox";
      env = [
         "XCURSOR_SIZE,24"   
         "HYPRCURSOR_SIZE,24"
      ];
      general = {
        gaps_in = 5;
        gaps_out = 20;
        border_size = 2;
        "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
        "col.inactive_border" = "rgba(595959aa)";
        resize_on_border = false;
        allow_tearing = false;
        layout = "dwindle";
      };
      decoration = {
        rounding = 10;

        # Change transparency of focused and unfocused windows
        active_opacity = 1.0;
        inactive_opacity = 1.0;

        drop_shadow = true;
        shadow_range = 4;
        shadow_render_power = 3;
        "col.shadow" = "rgba(1a1a1aee)";

        # https://wiki.hyprland.org/Configuring/Variables/#blur
        blur = {
            enabled = true;
            size = 3;
            passes = 1;
            vibrancy = 0.1696;
        };
      };
        animations =  {
            enabled = true;

            # Default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more

            bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";

            animation = [
            "windows, 1, 7, myBezier"
            "windowsOut, 1, 7, default, popin 80%"
            "border, 1, 10, default"
            "borderangle, 1, 8, default"
            "fade, 1, 7, default"
            "workspaces, 1, 6, default"
              ];
            };
          dwindle = {
            pseudotile = true; # Master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
            preserve_split = true; # You probably want this
          };
          master = {
            new_status = "master";
          };
            misc = { 
                force_default_wallpaper = -1; # Set to 0 or 1 to disable the anime mascot wallpapers
                disable_hyprland_logo = false; # If true disables the random hyprland logo / anime girl background. :(
            };

            #############
            ### INPUT ###
            #############

            # https://wiki.hyprland.org/Configuring/Variables/#input
            input = {
                kb_layout = "us";
                follow_mouse = 1;

                sensitivity = 0; # -1.0 - 1.0, 0 means no modification.

                touchpad = {
                    natural_scroll = false;
                };
            };

            # https://wiki.hyprland.org/Configuring/Variables/#gestures
            gestures = {
                workspace_swipe = false;
            };

            # Example per-device config
            # See https://wiki.hyprland.org/Configuring/Keywords/#per-device-input-configs for more
            device =  {
                name = "epic-mouse-v1";
                sensitivity = -0.5;
            };
            ####################
            ### KEYBINDINGSS ###
            ####################

            # See https://wiki.hyprland.org/Configuring/Keywords/
            "$mainMod" = "SUPER"; # Sets "Windows" key as main modifier

            # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
            bind = [
                    "$mainMod, Return, exec, $terminal"
                    "$mainMod, Q, killactive,"
                    "$mainMod, M, exit,"
                    "$mainMod, E, exec, $fileManager"
                    "$mainMod, F, togglefloating,"
                    "$mainMod, Space, exec, $menu"
                    "$mainMod, P, pseudo, # dwindle"
                    "$mainMod, S, togglesplit, # dwindle"
                    "$mainMod, B, exec, $browser"

            # Move focus with mainMod + arrow keys
                    "$mainMod, H, movefocus, l"
                    "$mainMod, L, movefocus, r"
                    "$mainMod, K, movefocus, u"
                    "$mainMod, L, movefocus, d"

            # Switch workspaces with mainMod + [0-9]
                    "$mainMod, 1, workspace, 1"
                    "$mainMod, 2, workspace, 2"
                    "$mainMod, 3, workspace, 3"
                    "$mainMod, 4, workspace, 4"
                    "$mainMod, 5, workspace, 5"
                    "$mainMod, 6, workspace, 6"
                    "$mainMod, 7, workspace, 7"
                    "$mainMod, 8, workspace, 8"
                    "$mainMod, 9, workspace, 9"
                    "$mainMod, 0, workspace, 10"

            # Move active window to a workspace with mainMod + SHIFT + [0-9]
                    "$mainMod SHIFT, !, movetoworkspace, 1"
                    "$mainMod SHIFT, @, movetoworkspace, 2"
                    "$mainMod SHIFT, Escape, movetoworkspace, 3"
                    "$mainMod SHIFT, $, movetoworkspace, 4"
                    "$mainMod SHIFT, %, movetoworkspace, 5"
                    "$mainMod SHIFT, ^, movetoworkspace, 6"
                    "$mainMod SHIFT, &, movetoworkspace, 7"
                    "$mainMod SHIFT, *, movetoworkspace, 8"
                    "$mainMod SHIFT, (, movetoworkspace, 9"
                    "$mainMod SHIFT, ), movetoworkspace, 10"

            # Example special workspace (scratchpad)
                    "$mainMod, A, togglespecialworkspace, magic"
                    "$mainMod SHIFT, A, movetoworkspace, special:magic"

            # Scroll through existing workspaces with mainMod + scroll
                    "$mainMod, mouse_down, workspace, e+1"
                    "$mainMod, mouse_up, workspace, e-1"
            ];
              # Move/resize windows with mainMod + LMB/RMB and dragging
              bindm = [
                        "$mainMod, mouse:272, movewindow"
                        "$mainMod, mouse:273, resizewindow"
              ];


              ##############################
            ### WINDOWS AND WORKSPACES ###
            ##############################

            # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
            # See https://wiki.hyprland.org/Configuring/Workspace-Rules/ for workspace rules

            # Example windowrule v1
            # windowrule = float, ^(kitty)$

            # Example windowrule v2
            # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$

            windowrulev2 = "suppressevent maximize, class:.*"; # You'll probably like this.
        };
    };
    home.sessionVariables = {
      NIXOS_OZONE_WL = "1";
      WLR_NO_HARDWARE_CURSORS = "1";
    };
}
