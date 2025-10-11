{ config, lib, pkgs, home-manager, ... }:

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
    nvtopPackages.full
    nix-index
    zip
    unzip
    fabric-ai
    dvc
    ffmpeg
    nixos-generators
    claude-code
    
  ];
}
