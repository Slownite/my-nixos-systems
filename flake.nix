{
  description = "Reproducible build of my different NixOS hosts";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs?ref=nixos-25.05"; };
    base16-schemes = {
	url="github:base16-project/base16-schemes";
	flake=false;
	};
    stylix = {
      url = "github:nix-community/stylix?ref=release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    }; 
     nvf = {
	url = "github:NotAShelf/nvf";
	inputs.nixpkgs.follows = "nixpkgs";
      };
    doom-emacs = {
          url = "github:marienz/nix-doom-emacs-unstraightened";
          # Optional, to download less. Neither the module nor the overlay uses this input.
          inputs.nixpkgs.follows = "";
    };
    home-manager = {
      url = "github:nix-community/home-manager?ref=release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, stylix, home-manager, base16-schemes, nvf, doom-emacs, ... }:
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
            inherit system stylix base16-schemes;
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
	  extraSpecialArgs = { inherit base16-schemes;};
          modules = [
            stylix.homeModules.stylix
	    nvf.homeManagerModules.default
            doom-emacs.homeModule
            ./hosts/home_station/home.nix
          ];
        };
      };
    };
}
