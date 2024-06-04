{
  description = "Flake for the C Worksheet Instrumentor";

  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    gradle2nix = {
      url = "github:tadfisher/gradle2nix/v2";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = {
    self,
    gradle2nix,
    nixpkgs,
    systems,
    ...
  } @ inputs: let
    forAllSystems = f: nixpkgs.lib.genAttrs (import systems) (system: f system);
  in {
    apps = forAllSystems (system: {
      c-worksheet-instrumentor = {
        type = "app";
        program = "${self.packages.${system}.c-worksheet-instrumentor}/bin/c-worksheet-instrumentor";
      };
      c-worksheet-server = {
        type = "app";
        program = "${self.packages.${system}.c-worksheet-instrumentor}/bin/c-worksheetify-server";
      };
    });

    devShells = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = pkgs.callPackage ./shell.nix {gradle2nix = gradle2nix.builders.${system};};
    });

    packages = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = self.packages.${system}.c-worksheet-instrumentor;

      c-worksheet-instrumentor = pkgs.callPackage ./c-worksheet-instrumentor.nix {inherit (gradle2nix.builders.${system}) buildGradlePackage;};
    });
  };
}
