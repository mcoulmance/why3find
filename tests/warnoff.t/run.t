  $ why3find prove test.mlw
  Warning, file "test.mlw", line 2, characters 6-7: unused variable x
  Theory test.Top: ✔ (-)
  Proved (test.mlw) ✔

  $ why3find prove --why3-warn-off unused_variable
  Theory test.Top: ✔ (-)
  Proved (test.mlw) ✔
