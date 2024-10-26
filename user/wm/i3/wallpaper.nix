{ config, lib, pkgs, ... }:

{
  options = {
    wallpaper = lib.mkOption {
      default = ../../theme/Cloudsnight.jpg;
      type = lib.types.path;
      description = ''
        Path to your wallpaper
      '';
    };
  };
  config = {
    home.file."wallpaper.sh".source = let
      script = pkgs.WriteShellScriptBin "wallpaper.sh" ''
        ${pkgs.feh}/bin/feh --bg-scale ${config.options.wallpaper}
      '';
    in "${script}/bin/wallpaper.sh";
  };
}
