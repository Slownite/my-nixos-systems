
{ config, pkgs, ... }:

{
    home.packages = [
        pkgs.python312
        pkgs.python312Packages.pip
        pkgs.python312Packages.virtualenv
    ];
}
