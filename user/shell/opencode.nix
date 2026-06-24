{
  config,
  pkgs,
  ...
}: let
  isMac = pkgs.stdenv.hostPlatform.isDarwin;

  # MacBook is left blank, PC switches to your local 100K profile
  defaultModel =
    if isMac
    then ""
    else "ollama/qwen3-coder-100k";
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
          # opencode does NOT auto-discover Ollama models for a custom
          # openai-compatible provider — only models listed here show up in the
          # TUI menu. Keys must match the names from `ollama list`.
          models = {
            "qwen3-coder-100k" = {tools = true;};
            "qwen3-coder-32k" = {tools = true;};
            "qwen3-8b-100k" = {tools = true;};
          };
        };

        huggingface = {
          npm = "@ai-sdk/openai-compatible";
          name = "Hugging Face Inference";
          options = {
            baseURL = "https://api-inference.huggingface.co/v1";
          };
          models = {
            "deepseek-ai/DeepSeek-V4-Pro" = {
              tools = true;
              thinking = "high";
            };
          };
        };
      };
    };
  };
}
