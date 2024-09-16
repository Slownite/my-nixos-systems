{ pkgs, ... }:
{
	      nixpkgs.config.allowUnfree = true;
            # Use the proprietary NVIDIA drivers
            services.xserver.videoDrivers = [ "nvidia" ];
            hardware.nvidia.modesetting.enable = true;

            # Ensure necessary firmware is included
            hardware.firmware = [
              pkgs.firmwareLinuxNonfree
            ];

            boot.extraModulePackages = [
              pkgs.linuxPackages.nvidia_x11
            ];
}
