  $ why3find prove -P alt-ergo -r --debug print-timings \
  >   | sed 's/[0-9][0-9]*/XX/g'
  Theory foo.S: ✔ (-)
  Theory foo.T: ✔ (XX)
  Proved (foo.mlw) ✔
  Digest prover          XX.XX (   XX) | XX.XX - XX.XX - XX.XX |
  Load driver            XX.XX (   XX) | XX.XX - XX.XX - XX.XX |
  Split theories         XX.XX (   XX) | XX.XX - XX.XX - XX.XX |
  Transformation         XX.XX (   XX) | XX.XX - XX.XX - XX.XX |
  Proofs dump            XX.XX (   XX) | XX.XX - XX.XX - XX.XX |
  Task checksum          XX.XX (   XX) | XX.XX - XX.XX - XX.XX |
  Prover autodetection   XX.XX (   XX) | XX.XX - XX.XX - XX.XX |
  Task preparation       XX.XX (   XX) | XX.XX - XX.XX - XX.XX |
  Task printing          XX.XX (   XX) | XX.XX - XX.XX - XX.XX |
  Launch proof           XX.XX (   XX) | XX.XX - XX.XX - XX.XX |
  CPU time               XX.XX
  Real time              XX.XX
