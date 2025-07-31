  $ export WHY3CONFIG=why3.conf

  $ cat > test.mlw <<EOF
  > theory T
  >   goal g : true
  > end
  > EOF

  $ why3find prove test.mlw
  Theory test.T: ✔ (-)

  $ cat test/proof.json
  {
    "profile": [],
    "proofs": { "T": { "g": { "tactic": "split_vc", "children": [] } } }
  }

  $ echo "" > test.mlw

  $ why3find prove test.mlw

  $ cat test/proof.json
  { "profile": [], "proofs": {} }
