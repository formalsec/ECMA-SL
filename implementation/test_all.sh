#!/bin/bash

BASE_COMMAND="./exec_test262_tests.sh -6 -r"
PARENT_DIR="my_tests/"
FILE_SUFIX="test_results.txt"
FILE_SUFIX_FAIL="test_results_fail.txt"
FILE_SUFIX_ERROR="test_results_error.txt"

REPLACE_QUERY='s|" ... [1m[0;31m[7mERROR[0m"||g'

echo "Testing Map ..."
${BASE_COMMAND} my_tests/Map | tee ${PARENT_DIR}map_${FILE_SUFIX}
grep FAIL ${PARENT_DIR}map_${FILE_SUFIX} > ${PARENT_DIR}map_${FILE_SUFIX_FAIL}
grep ERROR ${PARENT_DIR}map_${FILE_SUFIX} > ${PARENT_DIR}map_${FILE_SUFIX_ERROR}
rm -rf ${PARENT_DIR}map_${FILE_SUFIX}

# echo "Testing Number ..."
# ${BASE_COMMAND} my_tests/Number > ${PARENT_DIR}number_${FILE_SUFIX} &&\
# grep FAIL ${PARENT_DIR}number_${FILE_SUFIX} > ${PARENT_DIR}number_${FILE_SUFIX_FAIL}  &&\
# grep ERROR ${PARENT_DIR}number_${FILE_SUFIX} > ${PARENT_DIR}number_${FILE_SUFIX_ERROR}
# rm -rf ${PARENT_DIR}number_${FILE_SUFIX}
# 
# echo "Testing Math ..."
# ${BASE_COMMAND} my_tests/Math > ${PARENT_DIR}math_${FILE_SUFIX} &&\
# grep FAIL ${PARENT_DIR}math_${FILE_SUFIX} > ${PARENT_DIR}math_${FILE_SUFIX_FAIL} &&\
# grep ERROR ${PARENT_DIR}math_${FILE_SUFIX} > ${PARENT_DIR}math_${FILE_SUFIX_ERROR}
# rm -rf ${PARENT_DIR}math_${FILE_SUFIX}

# sed ${REPLACE_QUERY} ${PARENT_DIR}map_${FILE_SUFIX}
# sed ${REPLACE_QUERY} ${PARENT_DIR}map_${FILE_SUFIX_FAIL}
# sed ${REPLACE_QUERY} ${PARENT_DIR}map_${FILE_SUFIX_ERROR}
# 
# sed ${REPLACE_QUERY} ${PARENT_DIR}number_${FILE_SUFIX}
# sed ${REPLACE_QUERY} ${PARENT_DIR}number_${FILE_SUFIX_FAIL}
# sed ${REPLACE_QUERY} ${PARENT_DIR}number_${FILE_SUFIX_ERROR}
# 
# sed ${REPLACE_QUERY} ${PARENT_DIR}math_${FILE_SUFIX}
# sed ${REPLACE_QUERY} ${PARENT_DIR}math_${FILE_SUFIX_FAIL}
# sed ${REPLACE_QUERY} ${PARENT_DIR}math_${FILE_SUFIX_ERROR}
