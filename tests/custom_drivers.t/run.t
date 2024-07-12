  $ WHY3CONFIG=why3.conf why3find config -P alt-ergo
  Warning: Prover alt-ergo@2.4.2 has a custom driver ./custom.drv. Modifying this driver or one of its dependencies might leave the cache unsound, remember to clean it with 'rm -rf .why3find/alt-ergo@2.4.2'.
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo(2.4.2)
   - tactics: split_vc (depth 4)
  Why3find config saved to $TESTCASE_ROOT/why3find.json
  Summary:
  Warning: Prover alt-ergo@2.4.2 has a custom driver ./custom.drv. Modifying this driver or one of its dependencies might leave the cache unsound, remember to clean it with 'rm -rf .why3find/alt-ergo@2.4.2'.
  Emitted 1 warning

  $ WHY3CONFIG=why3.conf why3find prove
