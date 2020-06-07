#!/bin/bash

# This script runs an absolutely trivial smoke test; we'll replace it
# with something better in due course

set -eu

cd $(dirname $0)/..

PROG=bin/words
CONF=WORD.MDV

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

if [ ! -f ${CONF} ]; then
    cp test/${CONF}_template ${CONF}
    register-tmp ${CONF}
fi

# Initial smoke test
#
# If this fails, we don't waste time on other tests, and just let the
# whole thing crash.
$PROG 'rem acu tetigisti' | diff -q -- - test/expected.txt

# Main test(s)
create-tmp TMP_DISCREPANCIES

ignore-header () {
    tail -n +19
}

run-tests () {
    local test_file_name=test/aeneid_bk4
    local source=${test_file_name}.txt
    local expected=${test_file_name}.expected

    if [[ -v TRAVIS ]]; then
        create-tmp TMP_TRANSCRIPT
        $PROG < ${source} | ignore-header | tee $TMP_TRANSCRIPT
        diff -u -- - ${expected} < $TMP_TRANSCRIPT > $TMP_DISCREPANCIES
    else
        $PROG < ${source} | ignore-header | \
        diff -u -- - ${expected} > $TMP_DISCREPANCIES
    fi
}

if ! run-tests; then
  if [ -s "$TMP_DISCREPANCIES" ]; then
    cat $TMP_DISCREPANCIES
  fi
  echo FAIL
  exit 1
fi

echo PASS
