#!/bin/bash

TESTS_PARENT_DIR="my_tests/";
FILE_SUFIX="test_results.txt";
FILE_SUFIX_TEMP="test_results_temp.txt";

writeERROR() {
    echo "########## ERROR ##########" > $1;
}

writeFAIL() {
    echo "########## FAIL ##########" >> $1;
}

testSection() {
    ./exec_test262_tests.sh -6 -r my_tests/${1} > ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP};

    # Write all error tests to file	
    writeERROR ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX};
    grep $'\033\[1m\033\[0;31m\033\[7mERROR\033\[0m' ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP} | awk '{print $2}' >> ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX};

    # Adding a Line Break
    echo $'\n' >> ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX};

    # Write all failed tests to file
    writeFAIL ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX};
    grep $'\033\[1m\033\[0;31m\033\[5m\033\[7mFAIL\033\[0m' ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP} | awk '{print $2}' >> ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX};

    # Adding a Line Break
    echo $'\n' >> ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX};

    # Add last line to file
    tail -1 ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP} >> ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX};

    rm -rf ${TESTS_PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP};
}


for section in ArrayBuffer
do
    echo "Testing ${section} ..."
    testSection $section
done
