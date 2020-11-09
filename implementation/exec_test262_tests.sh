#!/bin/bash
printf " -------------------------------\n"
printf " \tECMA-SL JS Test262 Tool\n"
printf " -------------------------------\n"

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
  echo $1 >> test262_tests_result.md
}

function handleDirectory() {
  printf "Test Folder:\t $1\n\n"

  #echo "3. Loop over listed files"
  for f in $(ls $1*.js)
  do
    handleSingleFile $f
  done
}

function handleSingleFile() {
  FILENAME=$1
  echo "Testing ${FILENAME}"

  #echo "3.1. Copy contents to temporary file"
  cat "test/test262/environment/harness.js" > "test/main262.js"
  cat "${FILENAME}" >> "test/main262.js"

  #echo "3.2. Create the AST of the program in the file FILENAME and compile it to a \"Plus\" ECMA-SL program"
  cd "../JS2ECMA-SL"
  JS2ECMASL=$(node src/index.js -i ../implementation/test/main262.js -o test262_ast.esl)

  if [[ "${JS2ECMASL}" != "The file has been saved!" ]]
  then
    echo $JS2ECMASL
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"
    exit
  fi

  #echo "3.3. Copy compiled file to directory where to execute the tests"
  cp "test262_ast.esl" "../implementation/ES5_interpreter/test262_ast.esl"
  cd "../implementation"

  #echo "3.4. Compile program written in \"Plus\" to \"Core\""
  ECMALSLC=$(./main.native -mode c -i ES5_interpreter/test262.esl -o ES5_interpreter/core.esl)

  if [ $? -ne 0 ]
  then
    writeToMDFile "${FILENAME} | **ERROR** | ${ECMASLC}"
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"
    return
  fi

  #echo "3.5. Evaluate program and write the computed heap to the file heap.json. Output of the execution is written to the file result.txt"
  ECMASLCI=$(./main.native -mode ci -i ES5_interpreter/core.esl -h heap.json > result.txt)

  if [ $? -ne 0 ]
  then
    writeToMDFile "${FILENAME} | **ERROR** | ${ECMASLCI}"
    # echo "Check file result.txt"
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"
    return
  fi

  # 3.6. Check the result of the execution
  RESULT=$(tail -n 10 result.txt | grep "MAIN return -> ")

  if [[ "${RESULT}" =~ "MAIN return -> (\"C\", 'normal," ]]
  then
    printf "${BOLD}${GREEN}${INV}OK!${NC}\n"
    writeToMDFile "${FILENAME} | _OK_ | "
  else
    printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC}\n"
    writeToMDFile "${FILENAME} | **FAIL** | ${RESULT}"
  fi
}

function handleFiles() {
  files=($@)
  for file in "${files[@]}"; do
    handleSingleFile $file
  done
}

function handleDirectories() {
  directories=($@)
  for directory in "${directories[@]}"; do
    handleDirectory $directory
  done
}

function usage {
  echo "Usage: $(basename $0) [-df]" 2>&1
  echo '   -d   path to a directory containing test files. All the present test files are used.'
  echo '   -f   path to a single test file.'
  exit 1
}

if [[ ${#} -eq 0 ]]; then
   usage
fi

#echo "1. Create the file that will be compiled from \"Plus\" to \"Core\" in step 3.4."
echo "import \"ES5_interpreter/test262_ast.esl\";" > "ES5_interpreter/test262.esl"
echo "import \"ES5_interpreter/ESL_Interpreter.esl\";" >> "ES5_interpreter/test262.esl"
echo "function main() {
  x := buildAST();
  ret := JS_Interpreter_Program(x);
  return ret
}" >> "ES5_interpreter/test262.esl"


#echo "2. Compile the ECMA-SL language"
# OCAMLMAKE=$(make)
make

if [ $? -ne 0 ]
then
  # echo $OCAMLMAKE
  exit
fi

echo "" > test262_tests_result.md
# Write table header to MD file
writeToMDFile "File path | Result | Observations"
writeToMDFile "--- | --- | ---"

# Define list of arguments expected in the input
optstring=":df"

while getopts ${optstring} arg; do
  numarr=($@)
  unset numarr[0] # the first item of the array is the "arg". We don't want to pass it to the functions being called.
  case $arg in
    d) handleDirectories ${numarr[@]} ;;
    f) handleFiles ${numarr[@]} ;;

    ?)
      echo "Invalid option: -${OPTARG}."
      echo
      usage
      ;;
  esac
done


# 4. Remove temporary files previously created
rm "test/main262.js"
rm "ES5_interpreter/test262.esl"
rm "ES5_interpreter/test262_ast.esl"
rm "../JS2ECMA-SL/test262_ast.esl"
