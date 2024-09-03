  $ WHY3CONFIG=why3.conf why3find config -P alt-ergo -d 2 \
  >   -T split_vc,inline_goal
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo(2.4.2)
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json
  $ cat why3find.json
  {
    "time": 1,
    "depth": 2,
    "configs": [],
    "packages": [],
    "provers": [ "alt-ergo" ],
    "tactics": [ "split_vc", "inline_goal" ],
    "drivers": [],
    "warnoff": []
  }

  $ WHY3CONFIG=why3.conf why3find config
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo(2.4.2)
   - tactics: split_vc, inline_goal (depth 2)

  $ WHY3CONFIG=why3.conf why3find config --default
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo(2.4.2), z3(4.8.15)
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json

  $ WHY3CONFIG=why3.conf why3find config --strict
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo@2.4.2, z3@4.8.15
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json

  $ WHY3CONFIG=why3.conf why3find config --relax
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo(2.4.2), z3(4.8.15)
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json

  $ WHY3CONFIG=why3.conf why3find config -P 1:z3
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: z3(4.8.15), alt-ergo(2.4.2)
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json

  $ WHY3CONFIG=why3.conf why3find config -P 2:cvc4
  Warning: prover cvc4 not found (why3)
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: z3(4.8.15), (?cvc4), alt-ergo(2.4.2)
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json
  Summary:
  Warning: prover cvc4 not found (why3)
  Emitted 1 warning

  $ WHY3CONFIG=why3.conf why3find config -P 3:z3@2.0
  Warning: prover cvc4 not found (why3)
  Warning: prover z3@2.0 not found (why3)
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: (?cvc4), alt-ergo(2.4.2), (?z3@2.0)
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json
  Summary:
  Warning: prover cvc4 not found (why3)
  Warning: prover z3@2.0 not found (why3)
  Emitted 2 warnings

  $ WHY3CONFIG=why3.conf why3find config -P -cvc4
  Warning: prover z3@2.0 not found (why3)
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo(2.4.2), (?z3@2.0)
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json
  Summary:
  Warning: prover z3@2.0 not found (why3)
  Emitted 1 warning

  $ WHY3CONFIG=why3.conf why3find config --relax
  Configuration:
   - runner: 4 jobs, 1.0s
   - provers: alt-ergo(2.4.2), z3(4.8.15)
   - tactics: split_vc, inline_goal (depth 2)
  Why3find config saved to $TESTCASE_ROOT/why3find.json
