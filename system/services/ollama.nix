{ config, lib, pkgs, unstablePkgs, ... }:

{
  services.ollama = {
    # ollama-cuda's prebuilt binaries only target Turing+ (sm_75 and newer),
    # so the Pascal GTX 1080 Ti (compute capability 6.1) is skipped at startup
    # ("skipping CUDA device — compute capability not in compiled architectures")
    # and every model falls back to 100% CPU. Recompile the CUDA runner for
    # sm_61 so the GPU is actually used. CUDA 12.x still supports Pascal.
    package = unstablePkgs.ollama-cuda.override {
      cudaArches = [ "sm_61" ];
    };
    enable = true;
  };
 # services.open-webui = {
 #  enable = true;
 #  package = pkgs.open-webui;
 #};
}
