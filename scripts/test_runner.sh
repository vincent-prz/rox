#!/bin/bash

TEST_DIR="scripts/test_data"
EXEC_PATH="target/debug/rox"

cargo build

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
        echo "${filename}: KO"
        nb_failures=$((nb_failures + 1))
    fi
    nb_tests=$((nb_tests + 1))
done

rm actual

if [[ nb_failures -eq 0 ]]; then
    echo "All good, ${nb_tests} tests passed."
else
    echo "${nb_failures} tests failed out of ${nb_tests}."
fi