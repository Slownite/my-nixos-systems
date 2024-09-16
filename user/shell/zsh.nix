{ config, lib, pkgs, home-manager, ... }:

{
  programs.zsh = {
    oh-my-zsh = {
    enable = true;
    };
  };
  home.file = {
    ".zshrc".source = ./.zshrc;
  };
}
