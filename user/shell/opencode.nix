{ config, pkgs, ... }:

let
  isMac = pkgs.stdenv.hostPlatform.isDarwin;

  # MacBook is left blank, PC switches to your local 100K profile
  defaultModel = if isMac
    then ""
    else "ollama/qwen3-8b-100k";
in {
  programs.opencode = {
    enable = true;

    settings = {
      model = defaultModel;

      provider = {
        ollama = {
          npm = "@ai-sdk/openai-compatible";
          name = "Local Ollama";
          options = {
            baseURL = "http://localhost:11434/v1";
          };
          # All 3 variations are registered here so they appear cleanly in your TUI menu
          models = {
            "qwen3-coder-32k" = { tools = true; };
            "qwen3-coder-100k" = { tools = true; };
            "qwen3-8b-100k" = { tools = true; };
          };
        };

        openrouter = {
          npm = "@ai-sdk/openai-compatible";
          name = "OpenRouter Cloud";
          options = {
            baseURL = "https://openrouter.ai/api/v1";
          };
          models = {
            "deepseek/deepseek-v4-pro" = {
              tools = true;
              thinking = "high";
            };
          };
        };
      };
    };
  };
}
