{ config, lib, pkgs, ... }:

let
  epkgs = pkgs.emacsPackagesFor pkgs.emacs;

  # ---- Emacs with your packages ----
  joyEmacs = epkgs.emacsWithPackages (_:
    with epkgs; [
      use-package
      # completion
      corfu
      vertico
      orderless
      marginalia
     ############
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
    ]);
  packages = with pkgs; [

    nil
    #python
    basedpyright # or pyright
    ruff
    black
    #js
    vue-language-server
    biome
    typescript-language-server
    #cpp
    llvmPackages_21.clang-tools
    ripgrep
    fd
    git
  ];
in {
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    joyEmacs
    emacs-all-the-icons-fonts
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
    noto-fonts
    noto-fonts-color-emoji
    noto-fonts-cjk-sans
  ] ++ packages;
  home.file = {
    "./emacs" = {
      source = ./emacs;
      target = ".emacs.d";
      recursive = true;
    };
  };

}

