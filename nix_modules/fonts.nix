{ config, lib, pkgs, ... }:

{
fonts = {
  enableDefaultPackages = true;
  packages = with pkgs; [
    nerd-fonts
  ];

  fontconfig = {
    defaultFonts = {
      sansSerif = [ "0xProto" ];
      monospace = [ "Agave" ];
    };
  };
};
}
