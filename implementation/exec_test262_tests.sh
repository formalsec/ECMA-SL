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


function usage {
  echo "Usage: $(basename $0) [-dfr]" 2>&1
  echo '   -d   one or multiple paths to directories containing test files. All the present test files are used.'
  echo '   -f   one or multiple paths to test files.'
  echo '   -r   path to a direcotry containing test files. If the directory contains other direcotries, all the tests present in those directories are also executed.'
  exit 1
}

function writeToFile() {
  local output_file=$1
  local params=("$@")
  unset params[0] # is the output file

  for s in "${params[@]}"; do
    echo $s >> $output_file
  done
}

# Checks:
# - file is a valid ES5 test (search for the key "es5id" in the frontmatter)
# - file doesn't use the built-in eval function
# - file is not a negative test (search for the key "negative" in the frontmatter)
function checkConstraints() {
  FILENAME=$1
  declare -n ret=$2
  # check if it's a es5id test
  ises5id=$(awk '/es5id:/ {print $0}' $FILENAME)
  if [[ "${ises5id}" == "" ]]; then
    printf "${BOLD}${YELLOW}${BLINK2}${INV}NOT EXECUTED: not ES5 test${NC}\n"

    ret="${FILENAME} | **NOT EXECUTED** | Is not a ES5 test"
    return 1
  fi
  # check if it uses/contains a call the built-in eval function
  iseval=$(awk '/eval\(/ {print $0}' $FILENAME)
  if [[ "${iseval}" != "" ]]; then
    printf "${BOLD}${YELLOW}${BLINK2}${INV}NOT EXECUTED: eval test${NC}\n"

    ret="${FILENAME} | **NOT EXECUTED** | Is an \"eval\" test"
    return 1
  fi
  # check if it's a negative test
  isnegative=$(awk '/negative:/ {print $2}' $FILENAME)
  if [[ "${isnegative}" != "" ]]; then
    printf "${BOLD}${YELLOW}${BLINK2}${INV}NOT EXECUTED: negative test${NC}\n"

    ret="${FILENAME} | **NOT EXECUTED** | ${isnegative}"
    return 1
  fi

  return 0
}

function handleSingleFile() {
  declare -n result=$1
  # increment number of files being tested.
  incTotal

  FILENAME=$2
  printf "Testing ${FILENAME} ... "

  local checkConstraints_return=""
  checkConstraints $FILENAME "checkConstraints_return"
  if [[ $? -ne 0 ]]; then
    # increment number of tests not executed
    incNotExecuted

    result=("$checkConstraints_return")
    return
  fi

  #echo "3.1. Copy contents to temporary file"
  cat "test/test262/environment/harness.js" > "test/main262.js"
  cat "${FILENAME}" >> "test/main262.js"

  if [ $? -ne 0 ]; then
    exit 1
  fi

  #echo "3.2. Create the AST of the program in the file FILENAME and compile it to a \"Plus\" ECMA-SL program"
  cd "../JS2ECMA-SL"
  JS2ECMASL=$(node src/index.js -i ../implementation/test/main262.js -o test262_ast.esl)

  if [[ "${JS2ECMASL}" != "The file has been saved!" ]]
  then
    echo $JS2ECMASL
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    # increment number of tests with error
    incError

    result=("${FILENAME}" "**ERROR**" "$JS2ECMASL")

    # Go back to previous/default directory before returning
    cd "../implementation"
    return
  fi

  #echo "3.3. Copy compiled file to directory where to execute the tests"
  cp "test262_ast.esl" "../implementation/ES5_interpreter/test262_ast.esl"
  cd "../implementation"

  #echo "3.4. Compile program written in \"Plus\" to \"Core\""
  ECMALSLC=$(./main.native -mode c -i ES5_interpreter/test262.esl -o ES5_interpreter/core.esl)

  if [ $? -ne 0 ]
  then
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    # increment number of tests with error
    incError

    result=("${FILENAME}" "**ERROR**" "${ECMASLC}")
    return
  fi

  #echo "3.5. Evaluate program and write the computed heap to the file heap.json. Output of the execution is written to the file result.txt"
  ECMASLCI=$(./main.native -mode ci -i ES5_interpreter/core.esl -h heap.json > result.txt)

  if [ $? -ne 0 ]
  then
    # echo "Check file result.txt"
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    # increment number of tests with error
    incError

    result=("${FILENAME}" "**ERROR**" "${ECMASLCI}")
    return
  fi

  # 3.6. Check the result of the execution
  RESULT=$(tail -n 10 result.txt | grep "MAIN return -> ")

  if [[ "${RESULT}" =~ "MAIN return -> (\"C\", 'normal," ]]
  then
    printf "${BOLD}${GREEN}${INV}OK!${NC}\n"

    # increment number of tests successfully executed
    incOk

    result=("${FILENAME}" "_OK_" "")
    return
  else
    printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC}\n"

    # increment number of tests failed
    incFail

    result=("${FILENAME}" "**FAIL**" "${RESULT}")
    return
  fi

  echo "Not suppose to reach here."
  exit 1
}

function testFiles() {
  declare -n results=$1
  local files=($@)
  unset files[0] # corresponds to the results var.

  for file in "${files[@]}"; do
    local test_result=()
    if [ -f $file ]; then
      # Test file
      handleSingleFile "test_result" $file
    else
      echo "Ignoring \"$file\". It's not a valid file."
      continue
    fi

    # Write results to file
    # transform returned results in a string ready to be written to file
    local str="${test_result[0]}"
    for s in "${test_result[@]:1}"; do
      str+=" | "
      str+=$s
    done
    results+=("$str")
  done
}

function handleFiles() {
  local output_file=$1
  local files=($@)
  unset files[0]

  # Write header to file
  local params=()
  if [[ ${#files[@]} > 1 ]]; then
    params+=("## Testing multiple files")
  else
    params+=("## Testing single file")
  fi
  params+=("---")
  writeToFile $output_file "${params[@]}"

  local files_results=()
  testFiles "files_results" "${files[@]}"

  local params=()
  params+=("### Summary")
  params+=("OK | FAIL | ERROR | NOT EXECUTED | Total")
  params+=(":---: | :---: | :---: | :---: | :---:")
  params+=("$ok_tests | $fail_tests | $error_tests | $not_executed_tests | $total_tests")
  params+=("### Individual results")
  params+=("File path | Result | Observations")
  params+=("--- | :---: | ---")
  params+=("${files_results[@]}")

  writeToFile $output_file "${params[@]}"
}

function handleSingleDirectory() {
  local output_file=$1
  local dir=$2
  local lastChar=${dir: -1}
  if [[ $lastChar != "/" ]]; then
    dir=$dir"/"
  fi

  declare -n files_results=$3
  # Tests existence of JS files and avoids logging errors to the console.
  ls $dir*.js > /dev/null 2>&1
  if [[ $? -eq 0 ]]; then
    testFiles "files_results" "$(ls $dir*.js)"
  fi

  if [[ $RECURSIVE -ne 0 ]]; then
    # Tests existence of directories in this folder and avoids logging errors to the console.
    ls -d $dir*/ > /dev/null 2>&1
    if [[ $? -eq 0 ]]; then
      handleDirectories $output_file "$(ls -d $dir*/)"
    fi
  fi
}

function handleDirectories() {
  local output_file=$1
  local dirs=($@)
  unset dirs[0]

  for dir in "${dirs[@]}"; do
    if [ -d $dir ]; then
      # Save current state of the dir counters.
      declare -i curr_dir_ok_tests=$dir_ok_tests
      declare -i curr_dir_fail_tests=$dir_fail_tests
      declare -i curr_dir_error_tests=$dir_error_tests
      declare -i curr_dir_not_executed_tests=$dir_not_executed_tests
      declare -i curr_dir_total_tests=$dir_total_tests
      # Reset directories' counters
      resetDirCounters
      # Test directory
      local -a dir_results=()
      handleSingleDirectory $output_file $dir "dir_results"

      if [[ $dir_total_tests -ne 0 ]]; then
        local params=()
        params+=("## Summary of the tests executed in \"$dir\"")
        params+=("OK | FAIL | ERROR | NOT EXECUTED | Total")
        params+=(":---: | :---: | :---: | :---: | :---:")
        params+=("$dir_ok_tests | $dir_fail_tests | $dir_error_tests | $dir_not_executed_tests | $dir_total_tests")
        params+=("")
        params+=("### Individual results")
        params+=("File path | Result | Observations")
        params+=("--- | :---: | ---")
        params+=("${dir_results[@]}")
        params+=("")

        writeToFile $output_file "${params[@]}"
      fi

      # Set directories' counters to the sum of the values before handling directory and the values obtained after handling directory
      dir_ok_tests+=$curr_dir_ok_tests
      dir_fail_tests+=$curr_dir_fail_tests
      dir_error_tests+=$curr_dir_error_tests
      dir_not_executed_tests+=$curr_dir_not_executed_tests
      dir_total_tests+=$curr_dir_total_tests
    else
      echo "Ignoring \"$dir\". It's not a valid directory"
      continue
    fi
  done
}

function incTotal() {
  total_tests+=1
  dir_total_tests+=1
}

function incOk() {
  ok_tests+=1
  dir_ok_tests+=1
}

function incFail() {
  fail_tests+=1
  dir_fail_tests+=1
}

function incError() {
  error_tests+=1
  dir_error_tests+=1
}

function incNotExecuted() {
  not_executed_tests+=1
  dir_not_executed_tests+=1
}

function resetDirCounters() {
  dir_total_tests=0
  dir_ok_tests=0
  dir_fail_tests=0
  dir_error_tests=0
  dir_not_executed_tests=0
}

function initVars() {
  # Counters
  declare -g -i total_tests=0
  declare -g -i ok_tests=0
  declare -g -i fail_tests=0
  declare -g -i error_tests=0
  declare -g -i not_executed_tests=0
  # Counters used in the directories
  declare -g -i dir_total_tests=0
  declare -g -i dir_ok_tests=0
  declare -g -i dir_fail_tests=0
  declare -g -i dir_error_tests=0
  declare -g -i dir_not_executed_tests=0

  declare -g -a results=()

  declare -g -i RECURSIVE=0
  declare -g -r OUTPUT_FILE=results.md

  # Empty the contents of the output file
  cat /dev/null > $OUTPUT_FILE
}


#
# BEGIN
#
initVars

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
  exit 1
fi

echo ""

# Define list of arguments expected in the input
optstring=":dfr"

while getopts ${optstring} arg; do
  numarr=($@)
  unset numarr[0] # the first item of the array is the "arg". We don't want to pass it to the functions being called.
  case $arg in
    # r) handleRecursively $2 ;;
    r) RECURSIVE=1; handleDirectories $OUTPUT_FILE ${numarr[@]}; break;;
    d) handleDirectories $OUTPUT_FILE ${numarr[@]}; break;;
    f) handleFiles $OUTPUT_FILE ${numarr[@]}; break;;

    ?)
      echo "Invalid option: -${OPTARG}."
      echo ""
      usage
      ;;
  esac
done


# 4. Remove temporary files previously created
rm "test/main262.js"
rm "ES5_interpreter/core.esl"
rm "ES5_interpreter/test262.esl"
rm "ES5_interpreter/test262_ast.esl"
rm "../JS2ECMA-SL/test262_ast.esl"
