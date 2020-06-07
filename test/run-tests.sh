#!/bin/bash

# This script runs an absolutely trivial smoke test; we'll replace it
# with something better in due course

set -eu

cd $(dirname $0)/..

declare -a tmpfiles

cleanup () {
    local exit_val=$?
    for t in ${tmpfiles[@]}; do
      rm -f -- $t || true
    done
    exit $exit_val
}

trap cleanup EXIT

if [ ! -f WORD.MDV ]; then
    cp test/WORD.MDV_template WORD.MDV
    tmpfiles+=(WORD.MDV)
fi

bin/words 'rem acu tetigisti' | diff -q -- - test/expected.txt

# mktemp () is LSB:
which tempfile &> /dev/null || tempfile () { mktemp "$@"; }
TEMP=$(tempfile)
tmpfiles+=($TEMP)

run-tests () {
    set +u
    if [ "$TRAVIS" = "true" ]; then
        set -u
        TEMP2=$(tempfile)
        tmpfiles+=($TEMP2)
        bin/words < test/aeneid_bk4.txt | tail -n +19 | tee $TEMP2
        diff -u -- - test/aeneid_bk4.expected < $TEMP2 > $TEMP
        rv=$?
        return $rv
    else
        set -u
        bin/words < test/aeneid_bk4.txt | tail -n +19 | \
        diff -u -- - test/aeneid_bk4.expected > $TEMP
    fi
}
    
if ! run-tests; then
  rv=$?
  if [ -s "$TEMP" ]; then
    cat $TEMP
  fi
  echo FAIL
  exit $rv
fi

echo PASS
