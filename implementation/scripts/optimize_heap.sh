#!/usr/bin/env bash
printf " -------------------------------\n"
printf " \tECMA-SL Heap JSON file generation tool\n"
printf " -------------------------------\n"


# Warn that Loc.ml must be changed; prefix string used in the new_loc function
echo -e "1. Update module \"Loc.ml\": loc prefix to \"\$loc_global_\" in the function \"new_loc\"."
printf "(Press any key when it's done)"
read -n 1

echo ""
make


echo ""
printf "2. Include \"harness.js\" file in the generation?"
read -n 1 -p" [yn] " with_harness
echo ""
if [[ $with_harness == "y" ]]; then

  echo ""
  echo -e "2.1. Guarantee that the \"EnteringGlobalCode\" function in the \"ES5_interpreter/section\ 10/section_10.4.esl\" calls the function \"initGlobalObject\" and not the function \"optimizeInitGlobalObject\"."
  printf "(Press any key when it's done)"
  read -n 1

  echo "\"use strict\";" > "output/harness_strict.js"
  cat "test/test262/environment/harness.js" >> "output/harness_strict.js"
  cat "test/test262/environment/harness.js" > "output/harness.js"

  echo "2.2. Creating AST for the \"harness.js\" file (non-strict) ..."
  node ../JS2ECMA-SL/src/index.js -i "output/harness.js" -o "output/harness_ast.esl"
  if [ $? -ne 0 ]; then
    exit 1
  fi

  echo "2.3. Creating AST for the \"harness.js\" file (strict) ..."
  node ../JS2ECMA-SL/src/index.js -i "output/harness_strict.js" -o "output/harness_ast_strict.esl"
  if [ $? -ne 0 ]; then
    exit 1
  fi

  echo "import \"output/harness_ast_strict.esl\";" > "output/optimize_heap_strict.esl"
  echo "import \"output/harness_ast.esl\";" > "output/optimize_heap.esl"
  echo "import \"ES5_interpreter/ESL_Interpreter.esl\";" >> "output/optimize_heap_strict.esl"
  echo "import \"ES5_interpreter/ESL_Interpreter.esl\";" >> "output/optimize_heap.esl"
  echo -e 'function main() {
    x := buildAST();
    ret := JS_Interpreter_Program(x, null);
    return ret
  }' >> "output/optimize_heap_strict.esl"
  echo -e 'function main() {
    x := buildAST();
    ret := JS_Interpreter_Program(x, null);
    return ret
  }' >> "output/optimize_heap.esl"

else
  echo "import \"ES5_interpreter/ESL_Interpreter.esl\";" > "output/optimize_heap_strict.esl"
  echo "import \"ES5_interpreter/ESL_Interpreter.esl\";" > "output/optimize_heap.esl"
  echo -e 'function main() {
    globalObject := initGlobalObject(true);
    return globalObject
  }' >> "output/optimize_heap_strict.esl"
  echo -e 'function main() {
    globalObject := initGlobalObject(false);
    return globalObject
  }' >> "output/optimize_heap.esl"
fi


# Generate non-strict and strict json files.
echo ""
echo "3. Generating heap json files ..."

./main.native -mode c -i "output/optimize_heap_strict.esl" -o "output/core_optimize_heap_strict.esl" > /dev/null
if [ $? -ne 0 ]; then
  exit 1
fi
echo -e "\toutput/core_optimize_heap_strict.esl generated!"

./main.native -mode c -i "output/optimize_heap.esl" -o "output/core_optimize_heap.esl" > /dev/null
if [ $? -ne 0 ]; then
  exit 1
fi
echo -e "\toutput/core_optimize_heap.esl generated!"

./main.native -mode ci -i "output/core_optimize_heap_strict.esl" -h "globalHeap_strict.json" > /dev/null
if [ $? -ne 0 ]; then
  exit 1
fi
echo -e "\tglobalHeap_strict.json generated!"

./main.native -mode ci -i "output/core_optimize_heap.esl" -h "globalHeap.json" > /dev/null
if [ $? -ne 0 ]; then
  exit 1
fi
echo -e "\tglobalHeap.json generated!"


# Update json files
echo ""
echo "4. Updating generated json files: resolve errors guaranteeing that both are valid JSON files."

sh ./scripts/replace.sh


# Warn that changed performed in Loc.ml must be reverted
echo ""
echo "5. Revert module \"Loc.ml\" change performed in 1."
printf "(Press any key when it's done)"
read -n 1

echo ""
make


[ -f "output/harness_strict.js" ] && rm "output/harness_strict.js"
[ -f "output/harness.js" ] && rm "output/harness.js"
[ -f "output/harness_ast.esl" ] && rm "output/harness_ast.esl"
[ -f "output/harness_ast_strict.esl" ] && rm "output/harness_ast_strict.esl"
[ -f "output/optimize_heap.esl" ] && rm "output/optimize_heap.esl"
[ -f "output/optimize_heap_strict.esl" ] && rm "output/optimize_heap_strict.esl"
[ -f "output/core_optimize_heap.esl" ] && rm "output/core_optimize_heap.esl"
[ -f "output/core_optimize_heap_strict.esl" ] && rm "output/core_optimize_heap_strict.esl"

echo ""
echo "Done!"
