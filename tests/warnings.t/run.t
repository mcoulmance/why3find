  $ why3find prove test.mlw
  Warning, file "test.mlw", line 2, characters 2-6: termination of this expression cannot be proved, but there is no `diverges' clause in the outer specification
  File "test.mlw", line 1, characters 9-10: proof failed
  Goal Top.f: ✘
    f [VC for f]
  Theory test.Top: ✘
  Error: 1 unproved file
  [1]

  $ why3find config --why3-warn-off missing_diverges
  Configuration:
   - runner: 2 jobs, 1.0s
   - tactics: split_vc (depth 4)
  Why3find config saved to $TESTCASE_ROOT/why3find.json

  $ why3find prove test.mlw
  File "test.mlw", line 1, characters 9-10: proof failed
  Goal Top.f: ✘
    f [VC for f]
  Theory test.Top: ✘
  Error: 1 unproved file
  [1]
