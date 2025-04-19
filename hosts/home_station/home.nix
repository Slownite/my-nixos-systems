{ config, pkgs, pkgs-unstable, ... }:

{
  imports = [
    ../../user/shell/sh.nix
    ../../user/app/emacs.nix
    ../../user/app/nvim.nix
    ../../user/theme/theme.nix
    ../../user/wm/i3/i3.nix
    ../../user/pkgs/python.nix
    ../../user/pkgs/go.nix
    ../../user/app/alacritty.nix

  ];
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "sam";
  home.homeDirectory = "/home/sam";

  nixpkgs.config = { allowUnfree = true; };

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    pkgs.lazygit
    pkgs.lazydocker
    pkgs.bookworm
    pkgs.firefox
    pkgs.keepassxc
    pkgs.pika-backup
    pkgs.discord
    pkgs.vlc
    pkgs.slack
    pkgs.vscodium
    pkgs.cargo
    pkgs.htop
    pkgs.nvtopPackages.full
    pkgs.nerdfonts
    pkgs.nix-index
    pkgs.spotify
    pkgs.brave
    pkgs.localsend
    pkgs.zip
    pkgs.unzip
    pkgs.vscode-fhs
    pkgs.magic-wormhole
    pkgs.onlyoffice-bin_latest
    pkgs-unstable.fabric-ai
    pkgs.dvc
    pkgs.ffmpeg
    pkgs.nixos-generators
    pkgs.obsidian
    pkgs.networkmanagerapplet
    pkgs.zotero
    pkgs.audacity
  ];

  gtk.enable = true;
  qt.enable = true;

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by ager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/sam/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = { EDITOR = "emacs"; };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
