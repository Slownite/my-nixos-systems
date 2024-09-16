{
  description = "Reproductible build of my different nix os hosts";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.05";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
  };

  outputs = { self, nixpkgs, stylix, home-manager, ... }:
    let
      system =  "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
      {
        nixosConfigurations = {
          homeStation = nixpkgs.lib.nixosSystem {
            specialArgs = {inherit system;};
            modules = [
               stylix.nixosModules.stylix
              ./hosts/home_station/configuration.nix
            ];
          };
          };
	  homeConfigurations = {
	
          sam = home-manager.lib.homeManagerConfiguration {
	    inherit pkgs;
            modules = [
	      ./hosts/home_station/home.nix
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
