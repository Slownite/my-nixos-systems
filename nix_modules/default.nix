{ config, lib, pkgs, ... }:

{
  imports = [
    ./shell_cmd.nix
    ./experimental.nix
    ./build_essentials.nix
    ./docker.nix
    ./programs/default.nix
  ];
  shell_cmd.enable = lib.mkDefault true;
}
