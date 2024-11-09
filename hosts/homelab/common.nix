{config, lib, pkgs, ...}:
{

  users.users.sam = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [ tree ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID+NssKGri2NM1UCTAy68LvclzeFjJWKMECkJauMIPUs snfdiop@outlook.com"
    ];
  };

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
