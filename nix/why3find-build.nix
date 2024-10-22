{ why3find }:

why3find.overrideAttrs (attrs: {
  name = "why3find-build";

  checkPhase = ''
    if ! diff -q why3find.opam why3find.opam.old; then
      echo "why3find.opam is not up-to-date with dune-project"
      exit 1
    fi
  '';

  postInstall = ''
    tar -cf build.tar.gz .
    mv build.tar.gz $out
  '';
})
