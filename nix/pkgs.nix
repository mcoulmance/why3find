{ }:

let
  sources = import ./sources.nix {};
  ocamlOverlay = oself: osuper: {
    mlmpfr = oself.callPackage ./mlmpfr.nix {};
    alt-ergo = oself.callPackage ./alt-ergo.nix {};
    ocplib-simplex = oself.callPackage ./ocplib-simplex.nix {};
    why3 = oself.callPackage ./why3.nix {};
    why3find = oself.callPackage ./why3find.nix {};
    why3find-build = oself.callPackage ./why3find-build.nix {};
    why3find-test = oself.callPackage ./why3find-test.nix {};
  };
  overlay = self: super: {
    niv = (import sources.niv {}).niv;
    ocaml-ng = super.lib.mapAttrs (
      name: value:
        if builtins.hasAttr "overrideScope" value
        then value.overrideScope ocamlOverlay
        else value
    ) super.ocaml-ng;
    inherit (super.callPackage sources."gitignore.nix" {}) gitignoreSource;
    why3 = throw "don't use pkgs.why3 but ocaml-ng.ocamlPackages_4_XX.why3";
  };
  pkgs = import sources.nixpkgs {
    # alt-ergo
    config.allowUnfree = true;
    overlays = [ overlay ];
  };
in
pkgs
