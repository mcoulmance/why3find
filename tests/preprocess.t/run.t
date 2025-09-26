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
          "tactic": "compute_specified",
          "children": [ { "prover": "alt-ergo@2.4.2", "time": 0.004 } ]
        },
        "Mul.com": {
          "tactic": "compute_specified",
          "children": [ { "prover": "alt-ergo@2.4.2", "time": 0.0 } ]
        }
      }
    }
  }
