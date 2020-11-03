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

function writeToMDFile() {
  echo $1 >> simple_tests_result.md
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

echo "" > simple_tests_result.md
# Write table header to MD file
writeToMDFile "File path | Result | Observations"
writeToMDFile "--- | --- | ---"

# 3. Loop over listed files
for f in $(ls test/simple/*.js)
do
  if [[ $f =~ .*harness.* ]]
  then
    continue
  fi

  FILENAME=$f
  echo "Testing ${FILENAME}"

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
  ECMASLC=$(./main.native -mode c -i ES5_interpreter/test.esl -o ES5_interpreter/core.esl)

  if [ $? -ne 0 ]
  then
    writeToMDFile "${FILENAME} | **ERROR** | ${ECMASLC}"
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"
    continue
  fi

  # 3.5. Evaluate program and write the computed heap to the file heap.json.
  #    Output of the execution is written to the file result.txt
  ECMASLCI=$(./main.native -mode ci -i ES5_interpreter/core.esl -h heap.json > result.txt)

  if [ $? -ne 0 ]
  then
    writeToMDFile "${FILENAME} | **ERROR** | ${ECMASLCI}"
    # echo "Check file result.txt"
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"
    continue
  fi

  # 3.6. Check the result of the execution
  RESULT=$(tail -n 10 result.txt | grep "MAIN return -> ")

  if [[ "${RESULT}" == "MAIN return -> (\"C\", 'normal, true, 'empty)" ]]
  then
    printf "${BOLD}${GREEN}${INV}OK!${NC}\n"
    writeToMDFile "${FILENAME} | _OK_ | "
  else
    printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC}\n"
    writeToMDFile "${FILENAME} | **FAIL** | ${RESULT}"
  fi

done


# 4. Remove temporary files previously created
rm "test/main.js"
rm "ES5_interpreter/test.esl"
rm "ES5_interpreter/test_ast.esl"
rm "../JS2ECMA-SL/test_ast.esl"
