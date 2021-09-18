#!/bin/bash

BASE_COMMAND="./exec_test262_tests.sh -6 -r";
PARENT_DIR="my_tests/";
FILE_SUFIX="test_results.txt";
FILE_SUFIX_TEMP="test_results_temp.txt";

writeERROR() {
    echo "########## ERROR ##########" > $1;
}

writeFAIL() {
    echo "########## FAIL ##########" >> $1;
}

testSection() {
    ${BASE_COMMAND} my_tests/${1} > ${PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP};

    # Write all error tests to file	
    writeERROR ${PARENT_DIR}${1,,}_${FILE_SUFIX};
    grep $'\033\[1m\033\[0;31m\033\[7mERROR\033\[0m' ${PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP} | awk '{print $2}' >> ${PARENT_DIR}${1,,}_${FILE_SUFIX};

    # Adding a Line Break
    echo $'\n' >> ${PARENT_DIR}${1,,}_${FILE_SUFIX};

    # Write all failed tests to file
    writeFAIL ${PARENT_DIR}${1,,}_${FILE_SUFIX};
    grep $'\033\[1m\033\[0;31m\033\[5m\033\[7mFAIL\033\[0m' ${PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP} | awk '{print $2}' >> ${PARENT_DIR}${1,,}_${FILE_SUFIX};

    # Adding a Line Break
    echo $'\n' >> ${PARENT_DIR}${1,,}_${FILE_SUFIX};

    # Add last line to file
    tail -1 ${PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP} >> ${PARENT_DIR}${1,,}_${FILE_SUFIX};

    rm -rf ${PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP};
}


for section in Number
do
    echo "Testing ${section} ..."
    testSection $section
done
