{ pkgs-unstable, ... }:

{
  services.picom = {
    enable = true;
    activeOpacity = 0.9;
    inactiveOpacity = 0.7;
    fade = true;
    package = pkgs-unstable.picom;
    opacityRules = [
      "100:class_g = 'firefox'"
      "100:class_g = 'rofi'"
      "100:class_g = 'Rofi'"
      "100:class_g = 'i3lock'"
      "100:class_g = 'I3lock'"
      "100:fullscreen"

    ];
  };
}
