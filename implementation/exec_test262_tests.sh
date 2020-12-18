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
  echo -e "Usage: $(basename $0) [OPTION]... [-dfir]"
  echo -e '
  -d <dir>   Directory containing test files.
             All the tests available in the directory are executed.
  -f <file>  File to test.
  -i <file>  File containing the list of files to test.
  -r <dir>   Directory containing test files and/or directories.
             If the directories contain other directories, all the tests available in those directories are also executed.

  Options:
  -E         Enable logging to file the tests executed with errors. File is "errors.log"
  -F         Enable logging to file the failed tests. File is "failures.log"
  -O         Enable logging to file the passed tests. File is "oks.log"'
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

function logStatusToFiles() {
  if [ $LOG_ERRORS -eq 1 ]; then
    cat /dev/null > $LOG_ERRORS_FILE
    for error in "${log_errors_arr[@]}"; do
      echo "$error" >> $LOG_ERRORS_FILE
    done
  fi

  if [ $LOG_FAILURES -eq 1 ]; then
    cat /dev/null > $LOG_FAILURES_FILE
    for failure in "${log_failures_arr[@]}"; do
      echo "$failure" >> $LOG_FAILURES_FILE
    done
  fi

  if [ $LOG_OKS -eq 1 ]; then
    cat /dev/null > $LOG_OKS_FILE
    for ok in "${log_oks_arr[@]}"; do
      echo "$ok" >> $LOG_OKS_FILE
    done
  fi
}

# Checks:
# - file is a valid ES5 test (search for the key "es5id" in the frontmatter)
# - file doesn't use the built-in eval function
# - file is not a negative test (search for the key "negative" in the frontmatter)
function checkConstraints() {
  FILENAME=$1
  # check if it's a es5id test
  ises5id=$(awk '/es6id:|esid:/ {print $0}' $FILENAME)
  if [[ "${ises5id}" != "" ]]; then
    printf "${BOLD}${YELLOW}${BLINK2}${INV}NOT EXECUTED: not ES5 test${NC}\n"

    checkConstraints_return="${FILENAME} | **NOT EXECUTED** | Is not a ES5 test"
    return 1
  fi
  # check if it uses/contains a call the built-in eval function
  iseval=$(awk '/eval\(/ {print $0}' $FILENAME)
  if [[ "${iseval}" != "" ]]; then
    printf "${BOLD}${YELLOW}${BLINK2}${INV}NOT EXECUTED: eval test${NC}\n"

    checkConstraints_return="${FILENAME} | **NOT EXECUTED** | Is an \"eval\" test"
    return 1
  fi
  # check if it's a negative test
  isnegative=$(awk '/negative:/ {print $2}' $FILENAME)
  if [[ "${isnegative}" != "" ]]; then
    printf "${BOLD}${YELLOW}${BLINK2}${INV}NOT EXECUTED: negative test${NC}\n"

    checkConstraints_return="${FILENAME} | **NOT EXECUTED** | ${isnegative}"
    return 1
  fi

  return 0
}

function handleSingleFile() {
  # increment number of files being tested.
  incTotal

  FILENAME=$1
  printf "Testing ${FILENAME} ... "

  checkConstraints $FILENAME
  if [[ $? -ne 0 ]]; then
    # increment number of tests not executed
    incNotExecuted

    test_result=("$checkConstraints_return")
    return
  fi

  #echo "3.1. Copy contents to temporary file"
  cat /dev/null > "test/main262.js"
  if [[ $(awk '/flags: \[onlyStrict\]/ {print $0}' $FILENAME) != "" ]]; then
    echo "\"use strict\";" >> test/main262.js
  fi
  cat "test/test262/environment/harness.js" >> test/main262.js
  cat "${FILENAME}" >> test/main262.js

  if [ $? -ne 0 ]; then
    exit 1
  fi

  #echo "3.2. Create the AST of the program in the file FILENAME and compile it to a \"Plus\" ECMA-SL program"
  cd "../JS2ECMA-SL"
  JS2ECMASL=$(node src/index.js -i ../implementation/test/main262.js -o test262_ast.esl)
  cd "../implementation"

  if [[ "${JS2ECMASL}" != "The file has been saved!" ]]
  then
    echo $JS2ECMASL
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    # increment number of tests with error
    incError

    if [ $LOG_ERRORS -eq 1 ]; then
      log_errors_arr+=("$FILENAME")
    fi

    result=("${FILENAME}" "**ERROR**" "$JS2ECMASL")

    return
  fi

  #echo "3.3. Move compiled file to directory where to execute the tests"
  mv "../JS2ECMA-SL/test262_ast.esl" "ES5_interpreter/test262_ast.esl"

  #echo "3.4. Compile program written in \"Plus\" to \"Core\""
  ECMALSLC=$(./main.native -mode c -i ES5_interpreter/test262.esl -o ES5_interpreter/core.esl)

  if [ $? -ne 0 ]
  then
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    # increment number of tests with error
    incError

    if [ $LOG_ERRORS -eq 1 ]; then
      log_errors_arr+=("$FILENAME")
    fi

    test_result=("${FILENAME}" "**ERROR**" "${ECMASLC}")
    return
  fi

  #echo "3.5. Evaluate program and write the computed heap to the file heap.json. Output of the execution is written to the file result.txt"
  if [ $LOG_ENTIRE_EVAL_OUTPUT -eq 1 ]; then
    ECMASLCI=$(./main.native -mode ci -i ES5_interpreter/core.esl -h heap.json > result.txt)
    # 3.6. Check the result of the execution
    RESULT=$(tail -n 10 result.txt | grep "MAIN pc -> ")
  else
    ECMASLCI=$(./main.native -mode ci -i ES5_interpreter/core.esl -h heap.json | tail -n 10 > result.txt)
    # 3.6. Check the result of the execution
    RESULT=$(grep "MAIN pc -> " result.txt)
  fi

  if [[ "${RESULT}" == "" ]]; then
    # echo "Check file result.txt"
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    # increment number of tests with error
    incError

    if [ $LOG_ERRORS -eq 1 ]; then
      log_errors_arr+=("$FILENAME")
    fi

    test_result=("${FILENAME}" "**ERROR**" "${ECMASLCI}" "${duration_str}")
    return
  elif [[ "${RESULT}" =~ "MAIN pc -> (\"C\", 'normal," ]]; then
    printf "${BOLD}${GREEN}${INV}OK!${NC}\n"

    # increment number of tests successfully executed
    incOk

    if [ $LOG_OKS -eq 1 ]; then
      log_oks_arr+=("$FILENAME")
    fi

    test_result=("${FILENAME}" "_OK_" "" "${duration_str}")
    return
  else
    printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC}\n"

    # increment number of tests failed
    incFail

    if [ $LOG_FAILURES -eq 1 ]; then
      log_failures_arr+=("$FILENAME")
    fi

    test_result=("${FILENAME}" "**FAIL**" "${RESULT}" "${duration_str}")
    return
  fi

  echo "Not suppose to reach here."
  exit 1
}

function testFiles() {
  local files=($@)

  for file in "${files[@]}"; do
    if [ -f $file ]; then
      # Test file
      handleSingleFile $file
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
    files_results+=("$str")
  done
}

function handleFiles() {
  local output_file=$1
  local files=($@)
  unset files[0]

  # log evaluation output to a file
  if [[ ${#files[@]} -eq 1 ]]; then
    LOG_ENTIRE_EVAL_OUTPUT=1
  fi

  # Write header to file
  local params=()
  if [[ ${#files[@]} > 1 ]]; then
    params+=("## Testing multiple files")
  else
    params+=("## Testing single file")
  fi
  params+=("---")
  writeToFile $output_file "${params[@]}"

  testFiles "${files[@]}"

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

  # Tests existence of JS files and avoids logging errors to the console.
  ls $dir*.js > /dev/null 2>&1
  if [[ $? -eq 0 ]]; then
    testFiles "$(ls $dir*.js)"
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
      files_results=()
      handleSingleDirectory $output_file $dir

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


function processFromInputFile() {
  local INPUT_FILES=($@)

  handleFiles $OUTPUT_FILE "$(cat ${INPUT_FILES[@]})"

  logStatusToFiles
}

function processRecursively() {
  local dirs=($@)
  RECURSIVE=1

  handleDirectories $OUTPUT_FILE ${dirs[@]}

  logStatusToFiles
}

function processDirectories() {
  local dirs=($@)

  handleDirectories $OUTPUT_FILE ${dirs[@]}

  logStatusToFiles
}

#
# BEGIN
#
if [[ ${#} -eq 0 ]]; then
   usage
fi

# Initialise global variables
# Counters
declare -i total_tests=0
declare -i ok_tests=0
declare -i fail_tests=0
declare -i error_tests=0
declare -i not_executed_tests=0
# Counters used in the directories
declare -i dir_total_tests=0
declare -i dir_ok_tests=0
declare -i dir_fail_tests=0
declare -i dir_error_tests=0
declare -i dir_not_executed_tests=0

declare checkConstraints_return=""
declare -a results=()
declare -a files_results=()
declare -a test_result=()

declare -i RECURSIVE=0
declare -r OUTPUT_FILE="logs/results_$(date +%d%m%yT%H%M%S).md"

declare -i LOG_ENTIRE_EVAL_OUTPUT=0

declare -i LOG_ERRORS=0
declare -i LOG_FAILURES=0
declare -i LOG_OKS=0
declare -r LOG_ERRORS_FILE="logs/errors_$(date +%d%m%yT%H%M%S).log"
declare -r LOG_FAILURES_FILE="logs/failures_$(date +%d%m%yT%H%M%S).log"
declare -r LOG_OKS_FILE="logs/oks_$(date +%d%m%yT%H%M%S).log"
declare -a log_errors_arr=()
declare -a log_failures_arr=()
declare -a log_ok_arr=()

# Empty the contents of the output file
cat /dev/null > $OUTPUT_FILE
# Check that "logs" directory exists and, if not, create it
if [ ! -d "logs" ]; then
  mkdir logs
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
optstring=":EFOd:f:i:r:"

declare -a dDirs=() # Array that will contain the directories to use with the arg "-d"
declare -a fFiles=() # Array that will contain the files to use with the arg "-f"
declare -a iFiles=() # Array that will contain the files to use with the arg "-i"
declare -a rDirs=() # Array that will contain the directories to use with the arg "-r"

while getopts ${optstring} arg; do
  case $arg in
    E) LOG_ERRORS=1 ;;
    F) LOG_FAILURES=1 ;;
    O) LOG_OKS=1 ;;
    d) dDirs+=("$OPTARG") ;;
    f) fFiles+=("$OPTARG") ;;
    i) iFiles+=("$OPTARG") ;;
    r) rDirs+=("$OPTARG") ;;

    ?)
      echo "Invalid option: -${OPTARG}."
      echo ""
      usage
      ;;
  esac
done

if [ ${#dDirs[@]} -ne 0 ]; then
  processDirectories ${dDirs[@]}
fi

if [ ${#fFiles[@]} -ne 0 ]; then
  handleFiles $OUTPUT_FILE ${fFiles[@]}
fi

if [ ${#iFiles[@]} -ne 0 ]; then
  processFromInputFile ${iFiles[@]}
fi

if [ ${#rDirs[@]} -ne 0 ]; then
  processRecursively ${rDirs[@]}
fi
