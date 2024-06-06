{
  description = "Flake for the C Worksheet Instrumentor";

  inputs = {
    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
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
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    devenv,
    flake-parts,
    gradle2nix,
    nixpkgs,
    systems,
    treefmt-nix,
    ...
  } @ inputs: let
    forAllSystems = f: nixpkgs.lib.genAttrs (import systems) (system: f system);
    treefmtEval = forAllSystems (system: treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix);
    flake = {
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

      checks = forAllSystems (system: {
        formatting = treefmtEval.${system}.config.build.check self;
      });

      devShells = forAllSystems (system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        default = devenv.lib.mkShell {
          inherit inputs pkgs;

          modules = [
            (import ./devenv.nix)
          ];
        };
      });

      formatter = forAllSystems (system: treefmtEval.${system}.config.build.wrapper);

      packages = forAllSystems (system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        default = self.packages.${system}.c-worksheet-instrumentor;

        c-worksheet-instrumentor = pkgs.callPackage ./c-worksheet-instrumentor.nix {inherit (gradle2nix.builders.${system}) buildGradlePackage;};
      });
    };
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      inherit flake;

      systems = import systems;

      perSystem = {
        config,
        pkgs,
        system,
        ...
      }: {
      };
    };
}
