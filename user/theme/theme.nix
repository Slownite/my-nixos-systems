{ config, lib, pkgs, ... }:

{
  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
    polarity = "dark";
    image = ./flatppuccin_4k_macchiato.png;
    autoEnable = true;
    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Ice";

  fonts = {
    monospace = {
      package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono"];};
      name = "JetBrainsMono Nerd Font Mono";
    };
    sansSerif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Sans";
    };
    serif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Serif";
      };
      sizes = {
        terminal = 12;
      };
    };
  };
}