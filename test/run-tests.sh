#!/bin/bash

# This script runs an absolutely trivial smoke test; we'll replace it
# with something better in due course

set -eu

cd $(dirname $0)/..

PROG=bin/words

declare -a tmpfiles

register-tmp () {
    local tmpfile=$1
    tmpfiles+=($tmpfile)
}

# mktemp () is LSB:
which tempfile &> /dev/null || tempfile () { mktemp "$@"; }

create-tmp () {
    declare -n ref=$1
    local TMP=$(tempfile)
    register-tmp $TMP
    ref=$TMP
}

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
    register-tmp WORD.MDV
fi

$PROG 'rem acu tetigisti' | diff -q -- - test/expected.txt

create-tmp TEMP

ignore-header () {
    tail -n +19
}

run-tests () {
    local test_name=test/aeneid_bk4
    local source=${test_name}.txt
    local expected=${test_name}.expected

    set +u
    if [ "$TRAVIS" = "true" ]; then
        set -u
        create-tmp TEMP2
        $PROG < ${source} | ignore-header | tee $TEMP2
        diff -u -- - ${expected} < $TEMP2 > $TEMP
        rv=$?
        return $rv
    else
        set -u
        $PROG < ${source} | ignore-header | \
        diff -u -- - ${expected} > $TEMP
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
