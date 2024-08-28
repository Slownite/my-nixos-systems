{
  description = "Reproductible build of my different nix os hosts";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.05";
    stylix.url = "github:danth/stylix";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      systems = [
        "x86_64-linux"
      ];
      forEachSystem = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
      {
        nixosConfigurations = forEachSystem (system : {
          homeStation = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              ./hosts/home_station/configuration.nix
              inputs.stylix.nixosModules.stylix
            ];
            specialArgs = {};
            config = {
              allowUnfree = true;
             };
          };
          homelab = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              ./hosts/homelab/configuration.nix
            ];
            specialArgs = {};
            config = {
              allowUnfree = true;
             };
          };
          homeLaptop = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              ./hosts/home_laptop/configuration.nix
            ];
            specialArgs = {};
            config = {
              allowUnfree = true;
             };
          };
          workLaptop = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              ./hosts/work_laptop/configuration.nix
            ];
            specialArgs = {};
            config = {
              allowUnfree = true;
             };
          };
          workStation = nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              ./hosts/work_station/configuration.nix
            ];
            specialArgs = {};
            config = {
              allowUnfree = true;
             };
          };
        });
      };
}
