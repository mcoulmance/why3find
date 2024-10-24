{ why3find,
  why3find-build,
  ppxlib,
  bisect_ppx,
  why3,
  alt-ergo,
  cvc4,
  jq }:

why3find.overrideAttrs (attrs: {
  name = "why3find-test";

  src = why3find-build + "/build.tar.gz";
  sourceRoot = ".";

  buildPhase = "true";

  doCheck = true;

  checkInputs = [ ppxlib bisect_ppx ];
  nativeCheckInputs = attrs.nativeCheckInputs ++ [ bisect_ppx ];

  checkPhase = ''
    BISECT_FILE=$(pwd)/bisect \
    dune build -p why3find -j1 --force --instrument-with bisect_ppx @runtest @nixtests
    bisect-ppx-report cobertura report.xml
    bisect-ppx-report summary | sed -e 's/.*(\(1\?[0-9]\{2\}\.[0-9]\+%\))/Coverage: \1/' > coverage.txt
  '' ;

  installPhase = ''
    mkdir $out
    cp report.xml $out
    cp coverage.txt $out
  '';
})
