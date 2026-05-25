{ config, lib, pkgs, ... }:

{
  # Enables the teamviewerd daemon and installs the TeamViewer GUI client.
  # TeamViewer is unfree; allowUnfree is set in configuration.nix.
  services.teamviewer.enable = true;
}
