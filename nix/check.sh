#!/bin/sh

# Check indentation

set -eu

ret=0

echo "Check why3find.opam"
dune build
if diff -q why3find.opam why3find.opam.old
then
    rm -f why3find.opam.old
else
    echo "why3find.opam is not up-to-date with dune-project"
    ret=1
fi

if [ $ret -eq 0 ]; then
    echo "Done."
else
    echo "Errors."
fi

exit $ret
