{ config, lib, pkgs, ... }:

{
  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
    polarity = "dark";
    image = ./flatppuccin_4k_macchiato.png;
    autoEnable = true;
    fonts = {
        sansSerif = {
          package = pkgs.nerdfonts;
          name = "0xProto";
        };

        monospace = {
          package = pkgs.nerdfonts;
          name = "Agave";
        };

        emoji = {
          package = pkgs.noto-fonts-emoji;
          name = "Noto Color Emoji";
        };
      };
    };
}
