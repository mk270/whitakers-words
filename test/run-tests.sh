#!/bin/bash

# This script runs an absolutely trivial smoke test; we'll replace it
# with something better in due course

set -eu

cd $(dirname $0)/..

cleanup () {
    local exit_val=$?
    rm -f WORD.MDV
    exit $exit_val
}

if [ ! -f WORD.MDV ]; then
    cp test/WORD.MDV_template WORD.MDV
    trap cleanup EXIT
fi

bin/words 'rem acu tetigisti' | diff -q -- - test/expected.txt

TEMP=$(tempfile)
if ! ( bin/words < test/aeneid_bk4.txt | \
       diff -u -- - test/aeneid_bk4.expected > $TEMP ); then
  rv=$?
  if [ -s "$TEMP" ]; then
    cat $TEMP
  fi
  rm -f -- $TEMP
  echo FAIL
  exit $?
fi
rm -f -- $TEMP

echo PASS
