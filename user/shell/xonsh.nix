{ config, lib, pkgs, ... }:

{
  home.packages = [ pkgs.xonsh ];

  xdg.configFile."xonsh/rc.xsh".text = ''
    # History
    $XONSH_HISTORY_BACKEND = 'sqlite'
    $XONSH_HISTORY_SIZE = (50000, 'commands')
    $HISTCONTROL = {'ignoredups', 'ignoreerr'}

    # Aliases (mirrors fish/zsh)
    aliases['ll'] = 'ls -l'
    aliases['la'] = 'ls -la'
    aliases['cd'] = 'z'
    aliases['cat'] = 'bat'
    aliases['f'] = 'fzf'
    aliases['vi'] = 'nvim'
    aliases['vim'] = 'nvim'
    aliases['v'] = 'nvim'
    aliases['t'] = 'tmux'
    aliases['ta'] = 'tmux a'
    aliases['h'] = 'herdr'
    aliases['copy'] = 'xclip -selection clipboard'
    aliases['gc'] = 'claude "git commit and git push"'
    aliases['pi'] = 'pnpx @mariozechner/pi-coding-agent@latest'

    # vi keybindings
    $VI_MODE = True

    # Basics
    $EDITOR = 'antigravity'
    $VISUAL = $EDITOR
    $PAGER = 'less'

    # Disable terminal flow control so herdr's ctrl+s prefix works.
    import subprocess as _sp
    try:
        _sp.run(['stty', '-ixon'])
    except Exception:
        pass

    import os.path
    if os.path.exists('/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'):
        source-bash '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'

    execx($(${pkgs.zoxide}/bin/zoxide init xonsh), 'exec', __xonsh__.ctx, filename='zoxide')
  '';

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.zoxide.enable = true;
}
