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

bin/words < test/aeneid_bk4.txt | diff -q -- - test/aeneid_bk4.expected

echo PASS
