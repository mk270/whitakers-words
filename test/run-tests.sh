#!/bin/bash

# This script runs an absolutely trivial smoke test; we'll replace it
# with something better in due course

set -eux

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

run-tests () {
    set +u
    if [ "$TRAVIS" = "true" ]; then
        set -u
        TEMP2=$(tempfile)
        bin/words < test/aeneid_bk4.txt | tee $TEMP2
        diff -u -- - test/aeneid_bk4.txt.expected < $TEMP2 > $TEMP
        rv=$?
        rm -f -- $TEMP2 || true
        return $rv
    else
        set -u
        bin/words < test/aeneid_bk4.txt | \
        diff -u -- - test/aeneid_bk4.expected > $TEMP
    fi
}
    
if ! ( run-tests ); then
  rv=$?
  if [ -s "$TEMP" ]; then
    cat $TEMP
  fi
  rm -f -- $TEMP
  echo FAIL
  exit $rv
fi
rm -f -- $TEMP

echo PASS
