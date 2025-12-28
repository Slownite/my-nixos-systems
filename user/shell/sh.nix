{
  config,
  lib,
  pkgs,
  home-manager,
  ...
}:

{
  imports = [
    ./zsh.nix
    ./tmux.nix
    ./starship.nix
    ./fish.nix
    ./fastfetch.nix
    ./claude_code.nix
  ];
  home.packages = with pkgs; [
    bat
    wget
    curl
    fzf
    gh
    htop
    zoxide
    nix-index
    zip
    unzip
    ffmpeg
    nixos-generators
    claude-code

  ];
}
