{ config, pkgs, ... }:
{
  nixpkgs.config.allowUnfree = true;
  # Use the proprietary NVIDIA drivers
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    open = false;
    modesetting.enable = true;
    nvidiaSettings = true;
    # GTX 1080 Ti (Pascal) was dropped by the 595.xx branch (the 26.05
    # default), which fails with "No NVIDIA GPU found" and boots to a blank
    # screen. Pin the 580 legacy branch, the last to support Pascal.
    package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
  };
  # Ensure necessary firmware is included
  hardware.firmware = [
    pkgs.linux-firmware
  ];

  environment.systemPackages = with pkgs; [
    nvtopPackages.full
  ];
}
