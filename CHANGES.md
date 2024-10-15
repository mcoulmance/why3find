Unreleased
----------

- Rename config options -m and -v to --master and --velocity (#99, !127)
- Fix printing the same warning multiple times (#98, !126)
- Do not filter out unproved stuck goals from proof.json (#102, !121)
- Add option -H n to run the proof strategy up to step n (see README.md) (!118)
- Do not loose the proofs of not yet tried goals when why3find prove is
  interrupted (#12, !114)
- Add the possibility to log prover run results in JSON format with the
  --log-prover-results option (!92)
- And options --list-provers and --list-tactics to list available provers
  and tactics (transformations without arguments) (!96)
- Improve cache efficiency by hashing tasks before preparation (#93, !91)
- Fix reloading the driver each time we must run a goal (!91)
- Add config options --strict and --relax to add and remove explicit provers
  versions from configuration file (!90)
- Create a Why3find library and move some modules in it (!87)
- Show maximal prover time and maximal tactic depth at the end of proof (!83)
- Fix moving configuration items from command-line (#89, !85)
- Print floats with only 3 decimals in proof.json files (#87, !81)
- Allow to enable Why3 debug flags from command-line (--why3-debug) and
  disable warnings from command-line or why3find config (--why3-warn-off)
  (#88, !80)
- Remove support of extra config files (!79, !123)
- Load plugins, accept all registered input formats  (#84, #85, !77)

1.0 (2024-05-15)
----------------

- Initial release
