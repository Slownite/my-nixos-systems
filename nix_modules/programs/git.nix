{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;
    userName = "Slownite";
    userEmail = "snfdiop@outlook.com";
    aliases = {
      co = "commit";
      s = "status";
      sw = "switch";
      swc = "switch -c";
      a = "add";
      p = "push";
    };
    lfs.enable = true;
  };
}
