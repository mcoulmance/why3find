#!/bin/sh

# Check indentation

set -eu

ret=0

echo "Check indent"
SOURCES_MI=$(find src lib utils -name "*.ml" -or -name "*.mli")
for f in $SOURCES_MI
do
    ocp-indent $f -o $f.lint
    if diff -q $f $f.lint ;
    then
        rm -f $f.lint
    else
        echo "File $f not correctly indented"
        ret=1
    fi
done

# Check headers

echo "Check headers"
SOURCES_ML=$(find src lib utils -name "*.ml*")
SOURCES_TS=$(find vscode/src -name "*.ts*")
for f in $SOURCES_ML $SOURCES_TS
do
    cp $f $f.head
    headache -c headers.cfg -h HEADER $f
    if diff -q $f $f.head ;
    then
        rm -f $f.head
    else
        echo "File $f has incorrect header"
        ret=1
    fi
done

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
