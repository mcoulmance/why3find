  $ why3find config
  Configuration:
   - runner: 2 jobs, 1.0s
   - provers: alt-ergo(2.4.2)
   - tactics: compute_in_goal, split_vc (depth 4)
  $ rm -fr .why3find
  $ why3find prove -r
  Warning: Goal "merge": tactic 'split_vc' shadowed by 'compute_in_goal'
  File "mergesort.mlw", line 17, characters 11-16: proof failed
  Goal Mergesort.merge: ✘
    merge [VC for merge]
  Theory mergesort.Mergesort: ✘ (4/5)
  Error: 1 unproved file
  Summary:
  Warning: Goal "merge": tactic 'split_vc' shadowed by 'compute_in_goal'
  Error: 1 unproved file
  Emitted 1 warning, 1 error
  [1]
