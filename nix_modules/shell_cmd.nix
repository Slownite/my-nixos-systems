{ config, lib, pkgs, ... }:

{
  options = {
    shell_cmd.enable = lib.mkEnableOption "enables shell_cmd";
  };
  config = lib.mkIf config.shell_cmd.enable {

    environment.systemPackages = with pkgs; [
      neovim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      bat
      wget
      curl
      tmux
    ];
    programs.zsh = {
      enable = true;
    };
    programs.git = {
      enable = true;
    };
  };
}
