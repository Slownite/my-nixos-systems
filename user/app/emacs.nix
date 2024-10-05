{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    ripgrep
    fd
    zig
    shellcheck
    python312Packages.isort
    pipenv
    # python312Packages.nose
    python312Packages.python-lsp-server
    emacsPackages.pyimport
    black
    direnv
    nerdfonts
    python312Packages.pytest
    pyenv
    nixfmt-classic
    python312Packages.markdown
    nodejs_22
    ispell
    dockfmt
    python312Packages.pyflakes
    proselint
    llvmPackages_12.clang-tools
    poetry
    glslang
    shfmt
    zls
  ];
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
  home.file = {
    "./doom.d" = {
      source = ./doom;
      target = "/home/sam/.doom.d";
      recursive = true;
    };
  };
}

