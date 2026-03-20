  $ why3find config -P alt-ergo@2.3.3
  Warning: prover alt-ergo@2.3.3 not found
  Configuration:
   - runner: 2 jobs, 1.0s
   - provers: (?alt-ergo@2.3.3)
   - tactics: split_vc (depth 4)
  Why3find config saved to $TESTCASE_ROOT/why3find.json
  Summary:
  Warning: prover alt-ergo@2.3.3 not found
  Emitted 1 warning

  $ grep provers why3find.json
    "provers": [ "alt-ergo@2.3.3" ],

  $ why3find config --update
  Configuration:
   - runner: 2 jobs, 1.0s
   - provers: alt-ergo@2.4.2
   - tactics: split_vc (depth 4)
  Why3find config saved to $TESTCASE_ROOT/why3find.json
