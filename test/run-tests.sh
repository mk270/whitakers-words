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

report-result () {
    local test_name=$1
    local result=$2

    echo $test_name .. $result
}

failed=0

run-tests () {
    local test_name=01_aeneid
    local test_file_dir=test/${test_name}
    local source=${test_file_dir}/input.txt
    local expected=${test_file_dir}/expected.txt

    create-tmp TMP_TRANSCRIPT
    $PROG < ${source} | ignore-header > $TMP_TRANSCRIPT
    [[ -v TRAVIS ]] && cat $TMP_TRANSCRIPT

    if diff -u -- - ${expected} < $TMP_TRANSCRIPT > $TMP_DISCREPANCIES
    then
        report-result $test_name PASS
    else
        [ -s "$TMP_DISCREPANCIES" ] && cat $TMP_DISCREPANCIES
        report-result $test_name FAIL
        failed=1
    fi
}

run-tests
exit $failed
