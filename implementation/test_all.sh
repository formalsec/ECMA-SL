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

	writeERROR ${PARENT_DIR}${1,,}_${FILE_SUFIX};
	grep ERROR ${PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP} >> ${PARENT_DIR}${1,,}_${FILE_SUFIX};

	writeFAIL ${PARENT_DIR}${1,,}_${FILE_SUFIX};
	grep FAIL ${PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP} >> ${PARENT_DIR}${1,,}_${FILE_SUFIX};

	rm -rf ${PARENT_DIR}${1,,}_${FILE_SUFIX_TEMP};
}


for section in Map
do
	echo "Testing ${section} ..."
	testSection $section
done
