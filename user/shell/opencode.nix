{ config, pkgs, ... }:

let
  isMac = pkgs.stdenv.hostPlatform.isDarwin;

  # Laptop hits DeepSeek V4 via HuggingFace inference, PC switches to your local 100K profile
  defaultModel = if isMac
    then "huggingface/deepseek-ai/DeepSeek-V4"
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

        huggingface = {
          npm = "@ai-sdk/openai-compatible";
          name = "HuggingFace Inference";
          options = {
            baseURL = "https://router.huggingface.co/v1";
          };
          models = {
            "deepseek-ai/DeepSeek-V4" = {
              tools = true;
              thinking = "high";
            };
          };
        };
      };
    };
  };
}
