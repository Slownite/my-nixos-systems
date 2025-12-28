{ pkgs, ... }:
{
  nixpkgs.config.allowUnfree = true;
  # Use the proprietary NVIDIA drivers
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    open = false;
    modesetting.enable = true;
    nvidiaSettings = true;
  };
  # Ensure necessary firmware is included
  hardware.firmware = [
    pkgs.linux-firmware
  ];

  boot.extraModulePackages = [
    pkgs.linuxPackages.nvidia_x11
  ];

  environment.systemPackages = with pkgs; [
    nvtopPackages.full
  ];
}
