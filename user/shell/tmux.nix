{ config, lib, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    shell = "${pkgs.xonsh}/bin/xonsh";
    keyMode = "vi";
    prefix = "C-s";
    newSession = true;
    mouse = true;
    plugins = with pkgs; [
      tmuxPlugins.cpu
      tmuxPlugins.nord
      tmuxPlugins.tmux-fzf
    ];
  };
}
