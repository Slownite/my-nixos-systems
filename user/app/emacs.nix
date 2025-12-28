{
  config,
  lib,
  pkgs,
  ...
}:

let
  # Pick the base Emacs build depending on OS:
  # - macOS: "normal emacs"
  # - Linux: GTK emacs
  emacsPkg = if pkgs.stdenv.isDarwin then pkgs.emacs else pkgs.emacs-gtk;

  epkgs = pkgs.emacsPackagesFor emacsPkg;

  # ---- Emacs with your packages ----
  joyEmacs = epkgs.emacsWithPackages (
    _: with epkgs; [
      # completion
      corfu
      vertico
      orderless
      marginalia
      consult
      embark
      embark-consult
      which-key
      evil
      evil-collection
      evil-commentary
      general
      hl-todo
      rainbow-delimiters
      doom-themes
      doom-modeline
      all-the-icons
      dashboard
      treemacs
      treemacs-evil
      centaur-tabs
      magit
      nix-ts-mode
      vue-mode
      eglot
      org-roam
      org-modern
      vterm
      multi-vterm
      direnv
      cape
      consult-eglot
      gcmh
      no-littering
      helpful
      avy
    ]
  );

  packages = with pkgs; [
    nil
    # python
    basedpyright
    pyright
    ruff
    black
    # js
    vue-language-server
    biome
    typescript-language-server
    # cpp
    llvmPackages_21.clang-tools
    ripgrep
    fd
    git
    direnv
    alejandra
    # fonts
    emacs-all-the-icons-fonts
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
    noto-fonts
    noto-fonts-color-emoji
    noto-fonts-cjk-sans
  ];
in
{
  fonts.fontconfig.enable = true;

  # --- Emacs daemon + emacsclient (works on macOS via launchd, Linux via systemd user) ---
  services.emacs = {
    enable = true;
    package = joyEmacs;
    client.enable = true;
  };

  # Make `emacs` behave like `emacsclient` (both macOS + Linux)
  # -c: create a GUI frame
  # -a emacs: if daemon isn't running, start fallback emacs
  home.shellAliases = {
    emacs = "emacsclient -c -a emacs";
    e = "emacsclient -c -a emacs";
    et = "emacsclient -t -a emacs";
  };

  # For git/system tools/editor integration (also helps macOS GUI apps)
  home.sessionVariables = {
    EDITOR = "emacsclient -a emacs";
    VISUAL = "emacsclient -c -a emacs";
  };

  home.packages = [ joyEmacs ] ++ packages;

  home.file = {
    "./emacs" = {
      source = ./emacs;
      target = ".emacs.d";
      recursive = true;
    };
  };
}
