{ buildDunePackage,
  gitignoreSource,
  dune_3,
  dune-site,
  alt-ergo,
  menhirLib,
  mlmpfr,
  why3,
  zmq,
  zeromq,
  yojson,
  terminal_size,
  jq,
  cvc4 }:

buildDunePackage {
  pname = "why3find";
  version = "dev";
  duneVersion = "3";

  src = gitignoreSource ./..;

  buildInputs = [
    dune_3
    dune-site
    mlmpfr
    menhirLib
    why3
    zeromq
    zmq
    yojson
    terminal_size
  ];

  doCheck = true;

  # cvc4 is not strictly needed here but is used in @nixtests
  nativeCheckInputs = [ why3 alt-ergo jq cvc4 ] ;
}
