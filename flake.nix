{
  description = "Flake for the C Worksheet Instrumentor";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  }: let
    systems = [
      "aarch64-linux"
      "x86_64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
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

    packages = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = self.packages.${system}.c-worksheet-instrumentor;

      c-worksheet-instrumentor = pkgs.callPackage ./c-worksheet-instrumentor.nix {};
    });
  };
}
