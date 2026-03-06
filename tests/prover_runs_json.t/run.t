  $ why3find prove -P alt-ergo -r --log-prover-results results.json
  Theory foo.S: ✔ (-)
  Theory foo.T: ✔ (1)
  Proved (foo.mlw) ✔

  $ cat foo/proof.json
  {
    "profile": [ { "prover": "alt-ergo@2.4.2", "size": 16, "time": 0.1852 } ],
    "proofs": {
      "S": {},
      "T": {
        "Add.com": { "prover": "alt-ergo@2.4.2", "time": 0.0004 },
        "Mul.com": { "prover": "alt-ergo@2.4.2", "time": 0.0004 }
      }
    }
  }

  $ cat results.json | jq '(.time, .timeout, ."why3find time") |= "REDACTED"'
  {
    "goal": "<none>",
    "prover": "alt-ergo@2.4.2",
    "timeout": "REDACTED",
    "time": "REDACTED",
    "why3find time": "REDACTED",
    "result": "Valid"
  }
  {
    "goal": "Add.com",
    "prover": "alt-ergo@2.4.2",
    "timeout": "REDACTED",
    "time": "REDACTED",
    "why3find time": "REDACTED",
    "result": "Valid"
  }
