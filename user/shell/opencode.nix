{
  config,
  pkgs,
  ...
}: let
  isMac = pkgs.stdenv.hostPlatform.isDarwin;

  # MacBook is left blank, PC switches to local ornith model
  defaultModel =
    if isMac
    then ""
    else "ollama/ornith:9b";
in {
  programs.opencode = {
    enable = true;

    settings = {
      model = defaultModel;

      provider = {
        ollama = {
          npm = "@ai-sdk/openai-compatible";
          name = "Local";
          options = {
            baseURL = "http://localhost:11434/v1";
          };
          models = {
            "ornith:9b" = {tools = true;};
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
