{ config, lib, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
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
