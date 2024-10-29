The file why3find.json is the empty configuration. The default configuration
is non-empty. The why3find.json file is write protected in the sandbox. Thus,
if the command tries to change why3find.json, it will fail with a permission
error.
  $ why3find config --list-tactics > /dev/null
  why3find: $TESTCASE_ROOT/why3find.json: Permission denied
  [1]
