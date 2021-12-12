#!/bin/bash

TEST_DIR="scripts/test_data"
EXEC_PATH="target/debug/rox"
RED=$'\e[1;31m'
NOCOLOR=$'\033[0m'

cargo build
if [[ $? -ne 0 ]]; then
    echo "Couldn't build the interpreter, aborting."
    exit 1
fi

nb_tests=0
nb_failures=0
echo "=========="
for filename in $(ls ${TEST_DIR}/in); do
    input_file="${TEST_DIR}/in/${filename}"
    expected_output_file="${TEST_DIR}/out/${filename}"
    $EXEC_PATH $input_file > "actual"
    diff $expected_output_file actual > /dev/null
    if [[ $? -eq 0 ]]; then
        echo "${filename}: OK"
    else
        echo "${RED}${filename}: KO${NOCOLOR}"
        nb_failures=$((nb_failures + 1))
    fi
    nb_tests=$((nb_tests + 1))
done

rm actual

if [[ nb_failures -eq 0 ]]; then
    echo "All good, ${nb_tests} tests passed."
else
    echo "${nb_failures} tests failed out of ${nb_tests}."
    exit 1
fi