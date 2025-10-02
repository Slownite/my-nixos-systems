{config, pkgs, ...}:
{
  
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    pkgs.bookworm
    pkgs.floorp
    pkgs.keepassxc
    pkgs.pika-backup
    pkgs.discord
    pkgs.vlc
    pkgs.slack
    pkgs.vscodium
    pkgs.spotify
    pkgs.brave
    pkgs.localsend
    pkgs.vscode-fhs
    pkgs.onlyoffice-bin_latest
    pkgs.obsidian
    pkgs.networkmanagerapplet
    pkgs.zotero
    pkgs.audacity
    pkgs.protonvpn-gui
  ];
}
