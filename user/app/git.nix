{ config, pkgs, ... }:
{
  programs.git  = {
    enable = true;
    userEmail = "snfdiop@outlook.com";
    userName = "sam";
  };
}
