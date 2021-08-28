#!/bin/bash

BASE_COMMAND="./exec_test262_tests.sh -6 -r"
PARENT_DIR="my_tests/"
FILE_SUFIX="test_results.txt"
FILE_SUFIX_FAIL="test_results_fail.txt"
FILE_SUFIX_ERROR="test_results_error.txt"

REPLACE_QUERY='s|" ... [1m[0;31m[7mERROR[0m"||g'

${BASE_COMMAND} my_tests/Map | tee ${PARENT_DIR}map_${FILE_SUFIX} &&\
cat ${PARENT_DIR}map_${FILE_SUFIX} | grep FAIL | tee ${PARENT_DIR}map_${FILE_SUFIX_FAIL} &&\
cat ${PARENT_DIR}map_${FILE_SUFIX} | grep ERROR |tee ${PARENT_DIR}map_${FILE_SUFIX_ERROR}

${BASE_COMMAND} my_tests/Number | tee ${PARENT_DIR}number_${FILE_SUFIX} &&\
cat ${PARENT_DIR}number_${FILE_SUFIX} | grep FAIL |tee ${PARENT_DIR}number_${FILE_SUFIX_FAIL}  &&\
cat ${PARENT_DIR}number_${FILE_SUFIX} | grep ERROR |tee ${PARENT_DIR}number_${FILE_SUFIX_ERROR}

${BASE_COMMAND} my_tests/Math | tee ${PARENT_DIR}math_${FILE_SUFIX} &&\
cat ${PARENT_DIR}math_${FILE_SUFIX} | grep FAIL |tee ${PARENT_DIR}math_${FILE_SUFIX_FAIL} &&\
cat ${PARENT_DIR}math_${FILE_SUFIX} | grep ERROR |tee ${PARENT_DIR}math_${FILE_SUFIX_ERROR}


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