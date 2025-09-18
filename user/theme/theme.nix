{ config, lib, pkgs, base16-schemes, ... }:

{
  stylix = {
    enable = true;
    base16Scheme = "${base16-schemes}/nord.yaml";
    polarity = "dark";
    image = ./flatppuccin_4k_macchiato.png;
    autoEnable = true;
    targets.rofi.enable = false;
    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Ice";
    cursor.size = 24;
    fonts = {
	    serif = {
	      package = pkgs.dejavu_fonts;
	      name = "DejaVu Serif";
	    };

	    sansSerif = {
	      package = pkgs.dejavu_fonts;
	      name = "DejaVu Sans";
	    };

	    monospace = {
	      package = pkgs.dejavu_fonts;
	      name = "DejaVu Sans Mono";
	    };

	    emoji = {
	      package = pkgs.noto-fonts-emoji;
	      name = "Noto Color Emoji";
	    };
	  };
  };
}
