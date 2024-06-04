{pkgs, ...}: {
  projectRootFile = "flake.nix";
  programs.alejandra.enable = true;
  programs.scalafmt.enable = true;
}
