#!/bin/bash

# This script runs an absolutely trivial smoke test; we'll replace it
# with something better in due course

set -eu

cd $(dirname $0)/..

PROG=bin/words
CONF=WORD.MDV
TRIM=test/ignore-top-and-tail

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

# For belt-and-braces reasons, we try to delete all the temp files
# here as well
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

report-result () {
    local test_name=$1
    local result=$2

    echo $test_name .. $result
}


failed=0

run-test () {
    local test_name=$1
    local test_file_dir=test/${test_name}
    local source=${test_file_dir}/input.txt
    local expected=${test_file_dir}/expected.txt

    create-tmp TMP_DISCREPANCIES
    create-tmp TMP_TRANSCRIPT
    $PROG < ${source} | $TRIM > $TMP_TRANSCRIPT
    [[ -v TRAVIS ]] && cat $TMP_TRANSCRIPT

    if diff -u -- - ${expected} < $TMP_TRANSCRIPT > $TMP_DISCREPANCIES
    then
        report-result $test_name PASS
    else
        [ -s "$TMP_DISCREPANCIES" ] && cat $TMP_DISCREPANCIES >&2
        report-result $test_name FAIL
        failed=1
    fi

    # avoid buildup of temp files
    rm -f -- $TMP_TRANSCRIPT $TMP_DISCREPANCIES
}

all-test-names () {
    for name in test/[0-9][0-9]_*; do
        basename $name
    done
}

# Main test(s)
for name in $(all-test-names); do
    run-test $name
done

exit $failed
