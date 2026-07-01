{ config, lib, pkgs, unstablePkgs, ... }:

let
  llama-cpp-cuda = pkgs.llama-cpp.override {
    cudaSupport = true;
  };
in {
  environment.systemPackages = with pkgs; [
    llama-cpp-cuda
    llama-swap
  ];

  services.llama-swap = {
    enable = true;
    listenAddress = "127.0.0.1";
    port = 11434;
    openFirewall = false;
    settings = {
      models."ornith:9b" = {
        cmd = ''
          ${llama-cpp-cuda}/bin/llama-server \
            -hf deepreinforce-ai/Ornith-1.0-9B-GGUF \
            --port ''${PORT} \
            --host 127.0.0.1 \
            -ngl 9999 \
            -c 262144
        '';
        name = "Ornith 1.0 9B";
        description = "Agentic coding model";
      };
    };
  };
}
