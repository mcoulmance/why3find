## Linting

All files shall be properly indent with `ocp-indent` version `1.9.0`.
Typos are checked using `typos` with few exceptions listed in `./typos.toml`.

## Profiling with [landmarks](https://github.com/LexiFi/landmarks)

Why3find can be profiled with [landmarks](https://github.com/LexiFi/landmarks):

```sh
$ dune exec --workspace dune-workspace.profiling --context profiling -- why3find [COMMAND]
```

Note that this only profile why3find itself, and not its dependencies (in
particular this says nothing about Why3)
