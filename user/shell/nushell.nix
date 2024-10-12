{ config, lib, pkgs, pkgs-unstable, ... }:

{
  # Your existing home packages
  home.packages = with pkgs; [ carapace pueue ];

  # Enable and configure Pueue service
  services.pueue.enable = true;

  # Configure Nushell
  programs.nushell = {
    enable = true;
    extraConfig = ''
      { ||
        if (which direnv | is-empty) {
          return
        }

        direnv export json | from json | default {} | load-env
      }
      let carapace_completer = {|spans|
        carapace $spans.0 nushell $spans | from json
      }
      $env.config = {
        show_banner: false,
        completions: {
          case_sensitive: false # case-sensitive completions
          quick: true    # set to false to prevent auto-selecting completions
          partial: true    # set to false to prevent partial filling of the prompt
          algorithm: "fuzzy"    # prefix or fuzzy
          external: {
            # set to false to prevent nushell looking into $env.PATH to find more suggestions
            enable: true
            # set to lower can improve completion performance at the cost of omitting some options
            max_results: 100
            completer: $carapace_completer # check 'carapace_completer'
          }
        }
      }
      $env.PATH = ($env.PATH |
        split row (char esep) |
        prepend /home/${config.home.username}/.apps |
        prepend /home/${config.home.username}/.config/emacs/bin |
        append /usr/bin/env
      )
    '';
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
    package = pkgs-unstable.nushell;
  };

  # Configure Zoxide with Nushell integration
  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };
}
