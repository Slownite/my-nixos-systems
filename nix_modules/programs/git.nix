{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;

    lfs.enable = true;
    config = {

       userName = "Slownite";
       userEmail = "snfdiop@outlook.com";
	};
  };
}
