{ why3find,
  ppxlib,
  bisect_ppx }:

why3find.overrideAttrs (attrs: {
  name = "why3find-build";

  buildInputs = attrs.buildInputs ++ [ ppxlib bisect_ppx ];

  preBuild = ''
    export DUNE_INSTRUMENT_WITH=bisect_ppx
    cp why3find.opam why3find.opam.old
  '';

  doCheck = true;

  checkPhase = ''
    if ! diff -q why3find.opam why3find.opam.old; then
      echo "why3find.opam is not up-to-date with dune-project"
      exit 1
    fi
  '';

  installPhase = ''
    mkdir $out
    tar -cf $out/build.tar.gz .
  '';
})
