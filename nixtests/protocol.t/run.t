  $ export WHY3CONFIG=why3.conf

  $ why3find config --detect --master \
  >   | sed 's/[0-9]*ms/REDACTED/g; s/n=[0-9]*/REDACTED/g'
  warning: cannot read config file why3.conf:
    why3.conf: No such file or directory
  Found prover Alt-Ergo version 2.4.2, OK.
  Found prover Alt-Ergo version 2.4.2 (alternative: FPA)
  Found prover CVC4 version 1.8 (alternative: strings+counterexamples)
  Found prover CVC4 version 1.8 (alternative: strings)
  Found prover CVC4 version 1.8 (alternative: counterexamples)
  Found prover CVC4 version 1.8, OK.
  6 prover(s) added
  Save config to why3.conf
  Calibration:
   - alt-ergo@2.4.2   REDACTED REDACTED (master)
   - cvc4@1.8         REDACTED REDACTED (master)
  Configuration:
   - runner: 2 jobs, 1.0s
   - provers: alt-ergo(2.4.2), cvc4(1.8)
   - tactics: split_vc (depth 4)
  Why3find config saved to $TESTCASE_ROOT/why3find.json

  $ why3find server --address tcp://127.0.0.1:5555 &
  Address  tcp://127.0.0.1:5555
  Database $TESTCASE_ROOT/why3server
  Server is running…

  $ PIDSERVER=$!

  $ why3find worker --server tcp://localhost:5555 &
  Prover alt-ergo@2.4.2
  Prover cvc4@1.8
  Server tcp://localhost:5555
  Worker running…

  $ PIDWORKER=$!

  $ why3find prove --debug fake-local-timeouts --host localhost . \
  >   | sed 's/([0-9]*)/(REDACTED)/'
  Server tcp://localhost:5555
  Theory mergesort.Mergesort: ✔ (REDACTED)

  $ kill $PIDWORKER

  $ kill $PIDSERVER
