#!/bin/bash

# This script runs an absolutely trivial smoke test; we'll replace it
# with something better in due course

cd $(dirname $0)/..

bin/words 'rem acu tetigisti' | diff -q -- - test/expected.txt
