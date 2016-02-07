#!/bin/bash

# This script runs an absolutely trivial smoke test; we'll replace it
# with something better in due course

set -eu

cd $(dirname $0)/..

bin/words 'rem acu tetigisti' | diff -q -- - test/expected.txt

bin/words < test/aeneid_bk4.txt | diff -q -- - test/aeneid_bk4.expected

echo PASS
