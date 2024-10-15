  $ why3find prove -r foo.mlw bar.mlw
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  "foo.mlw", line 3, characters 8-11: proof failed
  Goal Add.com: ✘
    Add.com
  "foo.mlw", line 3, characters 8-11: proof failed
  Goal Mul.com: ✘
    Mul.com
  Library foo: ✘
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  "bar.mlw", line 3, characters 8-13: proof failed
  Goal Add.assoc: ✘
    Add.assoc
  "bar.mlw", line 4, characters 8-11: proof failed
  Goal Add.com: ✘
    Add.com
  "bar.mlw", line 3, characters 8-13: proof failed
  Goal Mul.assoc: ✘
    Mul.assoc
  Warning: prover alt-ergo@2.4.2 not configured (project)
  "bar.mlw", line 4, characters 8-11: proof failed
  Goal Mul.com: ✘
    Mul.com
  Library bar: ✘
  Error: 2 unproved files
  Summary:
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  Warning: prover alt-ergo@2.4.2 not configured (project)
  Error: 2 unproved files
  Emitted 5 warnings, 1 error
  [1]

  $ why3find config --check
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  Warning: prover alt-ergo@2.4.2 not configured (project)
  Configuration:
   - runner: 2 jobs, 1.0s
   - tactics: split_vc (depth 4)
  Summary:
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  Warning: prover alt-ergo@2.4.2 not configured (project)
  Emitted 5 warnings
  [1]
