#!/bin/bash
COLOR_RED='\033[0;31m'
COLOR_GREEN='\033[0;32m'
COLOR_NONE='\033[0m'

DIR_EXPECTED=testsuit/expected/
DIR_OUTPUT=bin/out/
DIR_DIFF=bin/diff/

if [ $# -ge 2 ]; then
    echo "Usage $(basename $0) <test_dir>"
    exit 1
fi

if [ $# = 1 ]; then
    input_dir=$1
    input_dir_name=$1
else
    input_dir=""
    input_dir_name="default"
fi

n_success=0
n_tests=0

echo "\n================================================================================"
echo "\t[TypesInterpreter] Test Suit"
echo "================================================================================\n"
echo "\tRunning all tests in the <${input_dir_name}> directory...\n"

make clean

for testfile in $(find testsuit/tests/${input_dir} -type f); do
    filename_test=$(echo ${testfile} | sed -r 's#^testsuit/tests/##' | sed -r 's#.js##')
    filename_expected=${DIR_EXPECTED}${filename_test}.out
    filename_out=${DIR_OUTPUT}${filename_test}.out
    filename_diff=${DIR_DIFF}${filename_test}.diff
    subdir=$(echo ${filename_test} | sed -r 's#/.*##')/
    
    mkdir -p ${DIR_OUTPUT}${subdir}
    mkdir -p ${DIR_DIFF}${subdir}
    
    $(sh $(dirname $0)/run.sh ${filename_test} --surpress >> ${filename_out} 2>&1)
    error_code=$?
    
    $(diff ${filename_out} ${filename_expected} >> ${filename_diff})
    n_diff=$(wc -c < ${filename_diff})

    if [ ${error_code} = 0 ] && [ ${n_diff} = 0 ]; then
        n_success=$(($n_success + 1))
        echo "${COLOR_GREEN}[SUCCESS]: ${filename_test} ${COLOR_NONE}"
    else
        echo "${COLOR_RED}[FAILURE]: ${filename_test} ${COLOR_NONE}"
    fi
    n_tests=$(($n_tests + 1))
done

echo "\n\t[$n_success/$n_tests] tests completed with success!\n"