#!/bin/bash

function main() {
	local prog=$1
	local output_dir=$2

	(test -z "$prog" || test -z "$output_dir") \
		&& printf "Usage: exec_es6_single_test.sh <program> <output_dir>\n" \
		&& return 1
	test -e $output_dir || mkdir -p $output_dir

	node ../JS2ECMA-SL/src/index.js -c -i $prog -o $output_dir/ast.cesl
	ECMA-SL -mode c -i ES6_interpreter/main2.esl -o $output_dir/core.cesl 
	echo "; " >> $output_dir/core.cesl 
	cat $output_dir/ast.cesl >> $output_dir/core.cesl
	ECMA-SL --verbose -mode ci -i $output_dir/core.cesl -s > $output_dir/result2.txt
	echo $?
#	rm $output_dir/core.cesl $output_dir/ast.cesl
	return 0
}

main $@
