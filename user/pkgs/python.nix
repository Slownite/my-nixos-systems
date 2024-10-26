{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    python312
    python312Packages.pip
    python312Packages.virtualenv
    python312Packages.isort
    pipenv
    python312Packages.python-lsp-server
    emacsPackages.pyimport
    black
    python312Packages.pytest
    pyenv
    python312Packages.pyflakes
  ];
}
