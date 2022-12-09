OUTPUT_DIR=outs
test -e $OUTPUT_DIR || mkdir -p $OUTPUT_DIR
node ../JS2ECMA-SL/src/index.js -c -i $1 -o $OUTPUT_DIR/ast.cesl
../ECMA-SL/main.native -mode c -i ES6_interpreter/main.esl -o $OUTPUT_DIR/core.cesl 
echo "; " >> $OUTPUT_DIR/core.cesl 
cat $OUTPUT_DIR/ast.cesl >> $OUTPUT_DIR/core.cesl
../ECMA-SL/main.native -mode ci -i $OUTPUT_DIR/core.cesl > $OUTPUT_DIR/result2.txt
rm $OUTPUT_DIR/core.cesl $OUTPUT_DIR/ast.cesl
