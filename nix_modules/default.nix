{ config, lib, pkgs, ... }:

{
  imports = [
    #./shell_cmd.nix
    #./experimental.nix
    #./build_essentials.nix
    #./docker.nix
    #./programs/default.nix
#    ./gaming.nix
#    ./theme.nix
  ];
}
