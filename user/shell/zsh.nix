
{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;

    history = {
      size = 50000;
      save = 50000;
      path = "${config.xdg.stateHome}/zsh/history"; # ~/.local/state/zsh/history
      share = true;
      ignoreDups = true;
    };

    # Keep aliases per-shell (mirrors fish)
    shellAliases = {
      ll = "ls -l";
      la = "ls -la";
      cd = "z";                     # zoxide jumping, like your fish alias
      cat = "bat";
      f = "fzf";
      vi = "nvim";
      vim = "nvim";
      v = "nvim";
      t = "tmux";
      ta = "tmux a";
      copy = "xclip -selection clipboard";
      ga = "git add";
      gc = "git commit -m";
      gp = "git push";
    };

    initExtra = ''
      # vi keybindings
      bindkey -v

      # Basics
      export EDITOR=nvim
      export VISUAL=$EDITOR
      export PAGER=less
    '';
  };

  # Shell helpers (Starship remains in starship.nix)
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
}
