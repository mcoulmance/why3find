  $ why3find prove -r foo.mlw bar.mlw
  Warning: prover foo not found
  Warning: prover bar not found
  File "foo.mlw", line 3, characters 15-42: proof failed
  Goal T.Add.com: ✘
  File "foo.mlw", line 3, characters 15-42: proof failed
  Goal T.Mul.com: ✘
  Library foo: ✘
  Warning: prover non-existing@1.0 not found
  File "bar.mlw", line 3, characters 17-60: proof failed
  Goal T.Add.assoc: ✘
  File "bar.mlw", line 4, characters 15-42: proof failed
  Goal T.Add.com: ✘
  File "bar.mlw", line 3, characters 17-60: proof failed
  Goal T.Mul.assoc: ✘
  Warning: prover alt-ergo@2.4.2 not found
  File "bar.mlw", line 4, characters 15-42: proof failed
  Goal T.Mul.com: ✘
  Library bar: ✘
  Error: 2 unproved files
  Summary:
  Warning: prover foo not found
  Warning: prover bar not found
  Warning: prover non-existing@1.0 not found
  Warning: prover alt-ergo@2.4.2 not found
  Error: 2 unproved files
  Emitted 4 warnings, 1 error
  [1]

  $ why3find config --check
  Warning: prover foo not found
  Warning: prover bar not found
  Warning: prover non-existing@1.0 not found
  Warning: prover alt-ergo@2.4.2 not found
  Configuration:
   - runner: 2 jobs, 1.0s
   - provers: (?foo), (?bar)
   - tactics: split_vc (depth 4)
  Summary:
  Warning: prover foo not found
  Warning: prover bar not found
  Warning: prover non-existing@1.0 not found
  Warning: prover alt-ergo@2.4.2 not found
  Emitted 4 warnings
  [1]
