  $ why3find prove -r foo.mlw
  Theory foo.S: ✔ (-)
  Theory foo.T: ✔ (2)

  $ cat foo/proof.json
  {
    "profile": [ { "prover": "alt-ergo@2.4.2", "size": 16, "time": 0.173 } ],
    "proofs": {
      "S": {},
      "T": {
        "Add.com": {
          "tactic": "split_vc",
          "children": [ { "prover": "alt-ergo@2.4.2", "time": 0.005 } ]
        },
        "Mul.com": {
          "tactic": "split_vc",
          "children": [ { "prover": "alt-ergo@2.4.2", "time": 0.005 } ]
        }
      }
    }
  }
