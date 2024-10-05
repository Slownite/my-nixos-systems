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
      unstable-pkgs = import nixpkgs-unstable {
    inherit system;
  };
      overlays = [
        (self: super: {
          nushell = unstable-pkgs.nushell;
        })
      ];
      pkgs = import nixpkgs {
            inherit system;
            overlays = overlays;
          };
    in {
      nixosConfigurations = {
      nixpkgs.overlays = overlays;
        homeStation = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit system; };
          modules = [
            home-manager.nixosModules.home-manager
            ./hosts/home_station/configuration.nix
          ];
        };
      };
      homeConfigurations = {
        sam = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules =
            [ stylix.homeManagerModules.stylix ./hosts/home_station/home.nix ];
        };
      };
      homelab = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit system; };
        modules = [ ./hosts/homelab/configuration.nix ];
      };
      homeLaptop = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit system; };
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
