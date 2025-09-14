{
  description = "Reproducible build of my different NixOS hosts";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs?ref=nixos-unstable"; };

    stylix = {
      url = "github:danth/stylix/release-unstable";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-unstable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.stylix.follows = "stylix";
    };
  };

  outputs = { self, nixpkgs, stylix, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in {
      nixosConfigurations = {
        homeStation = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit system stylix;
            # Add anything else you want available to modules here
          };
          modules = [
            home-manager.nixosModules.home-manager
            ./hosts/home_station/configuration.nix
          ];
        };
      };

      homeConfigurations = {
        sam = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            stylix.homeManagerModules.stylix
            ./hosts/home_station/home.nix
          ];
        };
      };
    };
}
