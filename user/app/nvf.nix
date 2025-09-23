{config, lib, pkgs, ...}:
{
  programs.nvf = {
    enable = true;
    # your settings need to go into the settings attribute set
    # most settings are documented in the appendix
    settings = {
      vim = {
      viAlias = true;
      vimAlias = true;
      statusline.lualine.enable = true;
      telescope.enable = true;
      autocomplete.nvim-cmp.enable = true;
      languages.python.enable = true;
      languages.nix.enable = true;
      treesitter.enable = true;
      lsp = {
        enable = true;
      };
     };
    };
  };
}
