{ pkgs ? import ./pkgs.nix {} }:

pkgs.mkShell {
  packages = [ pkgs.headache pkgs.ocamlPackages.ocp-indent ];
}
