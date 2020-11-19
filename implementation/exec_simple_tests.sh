#!/bin/bash
printf " -------------------------------\n"
printf " \tECMA-SL JS Test Tool\n"
printf " -------------------------------\n"
printf " Test Folder:\t test/simple\n\n"

RED='\033[0;31m'   	# RED
NC='\033[0m'       	# No Color
GREEN='\033[0;32m' 	# GREEN
YELLOW='\33[1;33m' 	# YELLOW
BLINK1='\e[5m'
BLINK2='\e[25m'	   	#BLINK
INV='\e[7m'         #INVERTED
LGREEN='\e[102m'
BOLD='\e[1m'


declare -i total_tests=0
declare -i ok_tests=0
declare -i fail_tests=0
declare -i error_tests=0

function writeToMDFile() {
  local INDIVIDUAL_RESULTS=("$@")
  ((last_idx=${#INDIVIDUAL_RESULTS[@]} - 1))
  local TEST_FOLDER=${INDIVIDUAL_RESULTS[last_idx]}
  unset INDIVIDUAL_RESULTS[last_idx]

  local FILE=simple_tests_result.md

  echo "## Report of the execution of the tests available in \"$TEST_FOLDER\"" > $FILE
  echo "### Summary" >> $FILE
  echo "OK | FAIL | ERROR | Total" >> $FILE
  echo "--- | --- | --- | ---" >> $FILE
  echo "$ok_tests | $fail_tests | $error_tests | $total_tests" >> $FILE
  echo "### Individual results" >> $FILE
  echo "File path | Result | Observations" >> $FILE
  echo "--- | --- | ---" >> $FILE

  for r in "${INDIVIDUAL_RESULTS[@]}" ; do
    echo "$r" >> $FILE
  done

  echo ""
  echo "MD file saved!"
}

# 1. Create the file that will be compiled from "Plus" to "Core" in step 3.4.
echo "import \"ES5_interpreter/test_ast.esl\";" > "ES5_interpreter/test.esl"
echo "import \"ES5_interpreter/ESL_Interpreter.esl\";" >> "ES5_interpreter/test.esl"
echo "function main() {
  x := buildAST();
  ret := JS_Interpreter_Program(x);
  return ret
}" >> "ES5_interpreter/test.esl"


# 2. Compile the ECMA-SL language
# OCAMLMAKE=$(make)
make

if [ $? -ne 0 ]
then
  # echo $OCAMLMAKE
  exit
fi

declare -a results=()
# 3. Loop over listed files
for f in $(ls test/simple/*.js); do

  if [[ $f =~ .*harness.* ]]
  then
    continue
  fi

  total_tests+=1

  FILENAME=$f
  printf "Testing ${FILENAME} ... "

  # 3.1. Copy contents to temporary file
  cat "test/simple/harness.js" > "test/main.js"
  cat "${FILENAME}" >> "test/main.js"

  # 3.2. Create the AST of the program in the file FILENAME and compile it to a "Plus" ECMA-SL program
  cd "../JS2ECMA-SL"
  JS2ECMASL=$(node src/index.js -i ../implementation/test/main.js -o test_ast.esl)

  if [[ "${JS2ECMASL}" != "The file has been saved!" ]]
  then
    echo $JS2ECMASL
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"
    exit
  fi

  # 3.3. Copy compiled file to directory where to execute the tests
  cp "test_ast.esl" "../implementation/ES5_interpreter/test_ast.esl"
  cd "../implementation"

  # 3.4. Compile program written in "Plus" to "Core"
  ECMASLC=$(./main.native -mode c -i ES5_interpreter/test.esl -o ES5_interpreter/test_core.esl)

  if [ $? -ne 0 ]
  then
    results+=("${FILENAME} | **ERROR** | ${ECMASLC}")
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    error_tests+=1
    continue
  fi

  # 3.5. Evaluate program and write the computed heap to the file heap.json.
  #    Output of the execution is written to the file result.txt
  ECMASLCI=$(./main.native -mode ci -i ES5_interpreter/test_core.esl -h heap.json > result.txt)

  if [ $? -ne 0 ]
  then
    results+=("${FILENAME} | **ERROR** | ${ECMASLCI}")
    # echo "Check file result.txt"
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    error_tests+=1
    continue
  fi

  # 3.6. Check the result of the execution
  RESULT=$(tail -n 10 result.txt | grep "MAIN return -> ")

  if [[ "${RESULT}" == "MAIN return -> (\"C\", 'normal, true, 'empty)" ]]
  then
    results+=("${FILENAME} | _OK_ | ")
    printf "${BOLD}${GREEN}${INV}OK!${NC}\n"

    ok_tests+=1
  else
    results+=("${FILENAME} | **FAIL** | ${RESULT}")
    printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC}\n"

    fail_tests+=1
  fi

done

writeToMDFile "${results[@]}" "/test/simple/"

# 4. Remove temporary files previously created
rm "test/main.js"
rm "ES5_interpreter/test.esl"
rm "ES5_interpreter/test_core.esl"
rm "ES5_interpreter/test_ast.esl"
rm "../JS2ECMA-SL/test_ast.esl"
