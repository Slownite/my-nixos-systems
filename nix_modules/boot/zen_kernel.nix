{ config, lib, pkgs, ... }:

{
  boot = {
    kernel.enable = true;
    kernelPackages = pkgs.linuxPackages_zen;
  };

}
