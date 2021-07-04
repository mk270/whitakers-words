#!/bin/bash

# This script runs an absolutely trivial smoke test; we'll replace it
# with something better in due course

set -eu

# This script creates a temporary directory, and only writes there.
# It can be called from any directory, but relative paths in arguments
# always start from the top source directory.
cd $(dirname $0)/..

# By default, test the fresh executable and data files located in
PROG=bin/words
datadir=.
# but these values may overridden.
for arg; do
    case "$arg" in
        --prog=*   )    PROG="${arg#--prog=}"    ;;
        --datadir=*) datadir="${arg#--datadir=}" ;;
        *)
            echo "$0: unknown argument '$arg'" >&2
            exit 1
            ;;
    esac
done

CONF=WORD.MDV
TRIM=test/ignore-top-and-tail

# This list of files required at run time is copied from HOWTO.txt.
DATA_FILES='INFLECTS.SEC ADDONS.LAT UNIQUES.LAT DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSFILE.GEN '

# Create a temporary directory and clean it at exit.
# Use mktemp if available, else obsolete tempfile.
tmpdir=`mktemp -d` || tmpdir=`tempfile -d`
trap 'rm -fr $tmpdir' EXIT

# Create a temporary datadir with the test configuration, mostly in
# order to select some non-interactive input options.
for f in $DATA_FILES; do
    ln -st $tmpdir $(realpath "$datadir/$f")
done
ln -st $tmpdir $(realpath test/$CONF)
export WHITAKERS_WORDS_DATADIR=$tmpdir

# Initial smoke test
#
# If this fails, we don't waste time on other tests, and just let the
# whole thing crash.
"$PROG" 'rem acu tetigisti' | diff -q - test/expected.txt

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
    local output=$tmpdir/output
    local trimmed=$tmpdir/trimmed

    "$PROG" < $source > $output
    $TRIM < $output > $trimmed
    if diff -q $trimmed $expected
    then
        report-result $test_name PASS
    else
        report-result $test_name FAIL
        echo '########## output:'
        cat $output
        echo '########## end of output'
        echo '########## diff between output and trimmed:'
        diff -u $output $trimmed || test $? = 1
        echo '########## end of diff between output and trimmed:'
        echo '########## diff between trimmed and expected:'
        diff -u $trimmed $expected || test $? = 1
        echo '########## end of diff between trimmed and expected:'
        failed=1
    fi
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
