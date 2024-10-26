{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ gopls gomodifytags gotests gore ];
  programs.go = { enable = true; };
}
