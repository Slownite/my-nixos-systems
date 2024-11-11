{config, lib, pkgs, ...}:
{

  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    git
    magic-wormhole
    figlet
    tmux
  ];
  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "no"; # Recommended for security
    settings.PasswordAuthentication = false; # Enforce key-based auth
  };
}
