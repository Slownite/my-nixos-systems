{ config, lib, pkgs, herdrPkgs, ... }:

# herdr — terminal workspace manager / agent multiplexer (tmux-like).
# https://github.com/ogulcancelik/herdr
#
# Configured to mirror user/shell/tmux.nix: panes live in xonsh, the prefix
# key matches tmux's `C-s`, and sessions persist (restoring agent panes).
let
  # Same shell tmux launches, so "everything lives in shell" the same way.
  herdrShell = "${pkgs.xonsh}/bin/xonsh";
in
{
  home.packages = [ herdrPkgs.default ];

  # herdr reads ~/.config/herdr/config.toml
  xdg.configFile."herdr/config.toml".text = ''
    [terminal]
    # Force xonsh in every pane, like tmux's default-command.
    default_shell = "${herdrShell}"
    shell_mode = "login"
    new_cwd = "follow"

    [keys]
    # Match the tmux prefix (C-s).
    prefix = "ctrl+s"

    # --- tmux-style keybinds (tab = tmux window, pane = tmux pane) ---
    # Panes (vi h/j/k/l focus already matches herdr defaults).
    split_vertical   = "prefix+%"     # tmux: split left/right
    split_horizontal = 'prefix+"'     # tmux: split top/bottom
    close_pane       = "prefix+x"     # tmux: kill pane
    zoom             = "prefix+z"     # tmux: zoom pane
    last_pane        = "prefix+;"     # tmux: last pane
    copy_mode        = "prefix+["     # tmux: copy mode

    # Tabs (tmux windows).
    new_tab      = "prefix+c"         # tmux: new window
    rename_tab   = "prefix+,"         # tmux: rename window
    close_tab    = "prefix+&"         # tmux: kill window
    previous_tab = "prefix+p"         # tmux: previous window
    next_tab     = "prefix+n"         # tmux: next window

    # Session.
    detach = "prefix+d"               # tmux: detach client

    [session]
    # tmux-style persistence: bring agent panes back on restore.
    resume_agents_on_restore = true
  '';
}
