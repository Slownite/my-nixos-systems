{
  description = "Reproducible build of my different NixOS hosts";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-25.11";
    };
    unstable-nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    base16-schemes = {
      url = "github:base16-project/base16-schemes";
      flake = false;
    };

    stylix = {
      url = "github:nix-community/stylix?ref=release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager?ref=release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    voxy = {
      url = "github:Slownite/voxy-linux";
    };
    ralf = {
    url = "github:Slownite/my_ralph_loop";
    };
  };

  outputs = { self, nixpkgs, stylix, home-manager, base16-schemes, unstable-nixpkgs, voxy, ralf, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      unstablePkgs = import unstable-nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      darwinSystem = "x86_64-darwin";
      darwinPkgs = import nixpkgs {
        system = darwinSystem;
        config.allowUnfree = true;
      };
    in {
      nixosConfigurations = {
        homeStation = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit system stylix base16-schemes;
          };
          modules = [
            voxy.nixosModules.voxy
  
            home-manager.nixosModules.home-manager
            ./hosts/home_station/configuration.nix
          ];
        };
      };

      homeConfigurations = {
        sam = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = { inherit base16-schemes; 
          inherit unstablePkgs;
          ralfPkgs = ralf.packages.x86_64-linux;
          };
          modules = [
            stylix.homeModules.stylix
            ./hosts/home_station/home.nix
          ];
        };

        mac = home-manager.lib.homeManagerConfiguration {
          pkgs = darwinPkgs;
          extraSpecialArgs = { inherit base16-schemes; 
            ralfPkgs = ralf.packages.x86_64-darwin;
          };
          modules = [

            stylix.homeModules.stylix
            ./hosts/macbook/home.nix
          ];
        };
	wsl = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = { inherit base16-schemes; };
          modules = [
            stylix.homeModules.stylix
            ./hosts/wsl/home.nix
          ];
        };
      };
    };
}
