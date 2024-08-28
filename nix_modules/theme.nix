{ config, lib, pkgs, ... }:

{
  options = {
    theme.enable = lib.mkEnableOption "enable theming using stylix";
  };
  config = lib.mkIf config.theme.enable {
    stylix.enable = true;
    stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/catpuccin-mocha.yaml";
    stylix.fonts = {
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
