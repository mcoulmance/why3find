  $ why3find prove -r foo.mlw bar.mlw
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  File "foo.mlw", line 3, characters 9-12: proof failed
  Goal T.Add.com: ✘
    Add.com
  File "foo.mlw", line 3, characters 9-12: proof failed
  Goal T.Mul.com: ✘
    Mul.com
  Library foo: ✘
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  File "bar.mlw", line 3, characters 9-14: proof failed
  Goal T.Add.assoc: ✘
    Add.assoc
  File "bar.mlw", line 4, characters 9-12: proof failed
  Goal T.Add.com: ✘
    Add.com
  File "bar.mlw", line 3, characters 9-14: proof failed
  Goal T.Mul.assoc: ✘
    Mul.assoc
  Warning: prover alt-ergo@2.4.2 not configured (project)
  File "bar.mlw", line 4, characters 9-12: proof failed
  Goal T.Mul.com: ✘
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
   - provers: (?foo), (?bar)
   - tactics: split_vc (depth 4)
  Summary:
  Warning: prover foo not found (why3)
  Warning: prover bar not found (why3)
  Warning: prover non-existing@1.0 not found (why3)
  Warning: prover non-existing@1.0 not configured (project)
  Warning: prover alt-ergo@2.4.2 not configured (project)
  Emitted 5 warnings
  [1]
