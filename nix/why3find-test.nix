{ why3find, why3find-build }:

why3find.overrideAttrs (attrs: {
  name = "why3find-test";
  src = why3find-build + "/build.tar.gz";
  sourceRoot = ".";
  preBuild = "";
  buildPhase = "true";
})
