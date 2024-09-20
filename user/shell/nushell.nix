{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    carapace
  ];
  programs.nushell = {
        enable = true;
	extraConfig = ''
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
       prepend /home/myuser/.apps |
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
  };
  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };
}
