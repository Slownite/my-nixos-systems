{ config, lib, pkgs, ... }:

{
  programs.fish = {
    enable = true;

    shellAliases = {
      ll = "ls -l";
      la = "ls -la";
      cd = "z";
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
  };
}
