{
  description = "Reproductible build of my different nix os hosts";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.05";
    stylix.url = "github:danth/stylix";
  };

  outputs = { self, nixpkgs, stylix, ... }:
    let
      system =  "x86_64-linux";
      pkgs = import nixpkgs {
       inherit system;
       config = {
        allowUnfree = true;
      };
     };
    in
      {
        nixosConfigurations = {
          homeStation = nixpkgs.lib.nixosSystem {
            specialArgs = {inherit system;};
            modules = [
              stylix.nixosModules.stylix
              ./hosts/home_station/configuration.nix
	   # Set a specific kernel version
            {
            #boot.kernelPackages = pkgs.linuxPackages_6_6; # or another version like pkgs.linuxPackages_6_1

            # Use the proprietary NVIDIA drivers
            services.xserver.videoDrivers = [ "nvidia" ];
            hardware.nvidia.modesetting.enable = true;

            # Ensure necessary firmware is included
            hardware.firmware = [
              pkgs.firmwareLinuxNonfree
            ];

            # Optional: Add kernel parameters to help with ACPI issues
            boot.kernelParams = [ "acpi=strict" ]; # or "acpi=off" if really needed

            # Additional modules for your network, USB, etc.
            boot.extraModulePackages = [
              pkgs.linuxPackages.nvidia_x11
              pkgs.linuxPackages.acpi_call
            ];

            # Adjust USB power settings if necessary
          }
            ];
          };
          };
          homelab = nixpkgs.lib.nixosSystem {
            specialArgs = {inherit system;};
            modules = [
              ./hosts/homelab/configuration.nix
            ];
          };
          homeLaptop = nixpkgs.lib.nixosSystem {
            specialArgs = {inherit system;};
            modules = [
              ./hosts/home_laptop/configuration.nix
            ];
          };
          workLaptop = nixpkgs.lib.nixosSystem {
            specialArgs = {inherit system;};
            modules = [
              ./hosts/work_laptop/configuration.nix
            ];
          };
          workStation = nixpkgs.lib.nixosSystem {
            specialArgs = {inherit system;};
            modules = [
              ./hosts/work_station/configuration.nix
            ];
          };
        };
      }
