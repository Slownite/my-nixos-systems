{
  description = "Reproductible build of my different nix os hosts";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs?ref=nixos-24.05"; };
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.stylix.follows = "stylix";
    };
  };

  outputs = { self, nixpkgs, stylix, home-manager, nixpkgs-unstable, ... }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      pkgs-unstable = import nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };
    in {
      nixosConfigurations = {
        homeStation = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit system;
            inherit pkgs-unstable;
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
          extraSpecialArgs = { inherit pkgs-unstable; };
          modules =
            [ stylix.homeManagerModules.stylix ./hosts/home_station/home.nix ];
        };
      };
      homelabHades = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit system;
          inherit pkgs-unstable;
        };
        modules = [ ./hosts/homelab/hades/configuration.nix ];
      };
      homelabZeus = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit system;
          inherit pkgs-unstable;
        };
        modules = [ ./hosts/homelab/zeus/configuration.nix ];
      };
      homelabPosseidon = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit system;
          inherit pkgs-unstable;
        };
        modules = [ ./hosts/homelab/posseidon/configuration.nix ];
      };
      homeLaptop = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit system;
          inherit pkgs-unstable;
        };
        modules = [ ./hosts/home_laptop/configuration.nix ];
      };
      workLaptop = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit system; };
        modules = [ ./hosts/work_laptop/configuration.nix ];
      };
      workStation = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit system; };
        modules = [ ./hosts/work_station/configuration.nix ];
      };
    };
}
