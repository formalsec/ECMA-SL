#!/usr/bin/env bash
printf " -------------------------------\n"
printf " \tECMA-SL JS Test262 Tool (non-optimised version)\n"
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

#
# BEGIN
#
if [[ ${#} -eq 0 ]]; then
   usage
fi

# Initialise global variables
declare now=$(date +%y%m%dT%H%M%S)
declare -i start_time=0
declare -i end_time=0
declare duration_str=""
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

declare NEGATIVE=""
declare -a results=()
declare -a files_results=()
declare -a test_result=()

declare -i RECURSIVE=0
declare -i WITH_PRE_COMPILED=0
declare -i WITH_HARNESS=0
declare OUTPUT_FOLDER="generated_ASTs/"
declare OUTPUT_FOLDER_WITH_HARNESS=$OUTPUT_FOLDER"with_harness/"
declare -r OUTPUT_FILE="logs/results_$now.md"

declare -i LOG_ENTIRE_EVAL_OUTPUT=0

declare -i SKIP_COMPILATION=0
declare -i LOG_ERRORS=0
declare -i LOG_FAILURES=0
declare -i LOG_OKS=0
declare -r LOG_ERRORS_FILE="logs/errors_$now.log"
declare -r LOG_FAILURES_FILE="logs/failures_$now.log"
declare -r LOG_OKS_FILE="logs/oks_$now.log"
declare -a log_errors_arr=()
declare -a log_failures_arr=()
declare -a log_ok_arr=()

# Empty the contents of the output file
cat /dev/null > $OUTPUT_FILE
# Check that "logs" directory exists and, if not, create it
if [ ! -d "logs" ]; then
  mkdir "logs"
fi
# Check that "output" directory exists and, if not, create it
if [ ! -d "output" ]; then
  mkdir "output"
fi

# Set OPAM environment variables
eval `opam env`

function compile() {
  #echo "1. Compile the ECMA-SL language"
  # OCAMLMAKE=$(make)
  make

  if [ $? -ne 0 ]
  then
    # echo $OCAMLMAKE
    exit 1
  fi

  #echo "2. Install JS2ECMA-SL dependencies"
  cd ../JS2ECMA-SL
  npm install
  cd ../implementation

  echo ""
}

# Define list of arguments expected in the input
optstring=":Si:"

declare -a iFiles=() # Array that will contain the files to use with the arg "-i"

while getopts ${optstring} arg; do
  case $arg in
    S) SKIP_COMPILATION=1 ;;
    i) iFiles+=("$OPTARG") ;;

    ?)
      echo "Invalid option: -${OPTARG}."
      echo ""
      usage
      ;;
  esac
done

if [ $SKIP_COMPILATION -ne 1 ]; then
  compile
fi


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

function writeToFile() {
  local output_file=$1
  local params=("$@")
  unset params[0] # is the output file

  for s in "${params[@]}"; do
    echo -e "$s" >> $output_file
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

function checkConstraints() {
  FILENAME=$1

  METADATA=$2
  # in case it's a negative test, save its expected error name in a variable to be used later.
  NEGATIVE=$(echo -e "$METADATA" | awk '/negative:/ {print $2}')

  return 0
}

function createMain262JSFile() {
  # echo "3.1. Copy contents to temporary file"
  cat /dev/null > "output/main262_$now.js"
  # Check if test is to run in strict code mode.
  if [[ $(echo -e "$2" | awk '/flags: \[onlyStrict\]/ {print $1}') != "" ]]; then
    echo "\"use strict\";" >> "output/main262_$now.js"
  fi

  cat "test/test262/environment/harness.js" >> "output/main262_$now.js"

  cat "$1" >> "output/main262_$now.js"

  if [ $? -ne 0 ]; then
    exit 1
  fi
}

function handleSingleFile() {
  # increment number of files being tested.
  incTotal

  FILENAME=$1
  printf "Testing ${FILENAME} ... "

  METADATA=$(cat "$FILENAME" | awk '/\/\*---/,/---\*\//')

  checkConstraints $FILENAME "$METADATA"

  # Copy contents to temporary file
  createMain262JSFile $FILENAME "$METADATA"

  # Create the AST of the program in the file FILENAME and compile it directly to \"Core\" ECMA-SL
  JS2ECMASL=$(time (node ../JS2ECMA-SL/src/index.js -i output/main262_$now.js -o output/test262_ast_$now.esl) 2>&1 1>&1)

  ast_duration_str=$(echo "$JS2ECMASL" | sed '$!d')

  # The asterisk is to ignore the timing statistics appended by the "time" command
  if [[ "$JS2ECMASL" != "The file has been saved!"* ]]; then
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    # increment number of tests with error
    incError

    if [ $LOG_ERRORS -eq 1 ]; then
      log_errors_arr+=("$FILENAME")
    fi

    ERROR_MESSAGE=$(echo -e "$JS2ECMASL" | head -n 1)

    test_result=("$FILENAME" "**ERROR**" "$ERROR_MESSAGE" "$ast_duration_str" "" "")
    return
  fi

  #echo "3. Create the file that will be compiled from \"Plus\" to \"Core\" in step 3.4."
  echo "import \"output/test262_ast_$now.esl\";" > "output/test262_$now.esl"

  sed '1d' "ES5_interpreter/plus.esl" >> "output/test262_$now.esl"

  # Compile program written in \"Plus\" to \"Core\
  ECMASLC=$(time (./main.native -mode c -i output/test262_$now.esl -o output/core_$now.esl) 2>&1 1>/dev/null)

  EXIT_CODE=$?

  plus2core_duration_str=$(echo "$ECMASLC" | sed '$!d')

  if [ $EXIT_CODE -ne 0 ]; then
    printf "${BOLD}${RED}${INV}ERROR${NC} during compilation of the \"ES5_interpreter/plus.esl\" file\n"

    echo -e "$ECMASLC" | sed '$d'

    exit 2
  fi

  # Evaluate program and write the computed heap to the file heap.json."
  local toggle_silent_mode=""
  [ $LOG_ENTIRE_EVAL_OUTPUT -eq 0 ] && toggle_silent_mode="-s"
  ECMASLCI=$(time (./main.native -mode ci -i output/core_$now.esl $toggle_silent_mode) 2>&1 1>&1)

  local EXIT_CODE=$?

  duration_str=$(echo "$ECMASLCI" | sed '$!d')
  # Remove timing statistics appended by the "time" command
  ECMASLCI=$(echo "$ECMASLCI" | sed '$d')

  # In case of a negative test, we're expecting to have a failure in the interpretation
  if [ -n "$NEGATIVE" ]; then
    # the "print substr" part of the piped awk command is to remove the double-quotes present in the string we're extracting
    local completion_value=$(echo -e "$ECMASLCI" | tail -n 10 | awk -F ", " '/MAIN pc ->/ {print substr($3, 2, length($3)-2)}')
    if [[ $NEGATIVE == $completion_value ]]; then
      printf "${BOLD}${GREEN}${INV}OK!${NC}\n"

      # increment number of tests successfully executed
      incOk

      if [ $LOG_OKS -eq 1 ]; then
        log_oks_arr+=("$FILENAME")
      fi

      test_result=("$FILENAME" "_OK_" "" "$ast_duration_str" "$plus2core_duration_str" "$duration_str")
    else
      printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC}\n"

      # increment number of tests failed
      incFail

      if [ $LOG_FAILURES -eq 1 ]; then
        log_failures_arr+=("$FILENAME")
      fi

      test_result=("$FILENAME" "**FAIL**" "$RESULT" "$ast_duration_str" "$plus2core_duration_str" "$duration_str")
    fi
  elif [ $EXIT_CODE -eq 0 ]; then
    printf "${BOLD}${GREEN}${INV}OK!${NC}\n"

    # increment number of tests successfully executed
    incOk

    if [ $LOG_OKS -eq 1 ]; then
      log_oks_arr+=("$FILENAME")
    fi

    test_result=("$FILENAME" "_OK_" "" "$ast_duration_str" "$plus2core_duration_str" "$duration_str")
  elif [ $EXIT_CODE -eq 1 ]; then
    printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC}\n"

    # increment number of tests failed
    incFail

    if [ $LOG_FAILURES -eq 1 ]; then
      log_failures_arr+=("$FILENAME")
    fi

    test_result=("$FILENAME" "**FAIL**" "$RESULT" "$ast_duration_str" "$plus2core_duration_str" "$duration_str")
  else
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    # increment number of tests with error
    incError

    if [ $LOG_ERRORS -eq 1 ]; then
      log_errors_arr+=("$FILENAME")
    fi

    ERROR_MESSAGE=$(echo -e "$ECMASLCI" | tail -n 1)

    test_result=("$FILENAME" "**ERROR**" "$ERROR_MESSAGE" "$ast_duration_str" "$plus2core_duration_str" "$duration_str")
  fi

  if [ $LOG_ENTIRE_EVAL_OUTPUT -eq 1 ]; then
    # Output of the execution is written to the file result.txt
    echo "Writing interpretation output to file..."
    echo "$ECMASLCI" > result.txt
  fi
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
  params+=("File path | Result | Observations | Creation of the AST | Plus to Core | Interpretation")
  params+=("--- | :---: | :---: | :---: | :---: | :---:")
  params+=("${files_results[@]}")

  writeToFile $output_file "${params[@]}"
}

function processFromInputFile() {
  local INPUT_FILES=($@)

  handleFiles $OUTPUT_FILE "$(cat ${INPUT_FILES[@]})"

  logStatusToFiles
}

function process() {
  # Environment variable used by the "time" tool
  TIMEFORMAT='%3lR'


  if [ ${#iFiles[@]} -ne 0 ]; then
    processFromInputFile ${iFiles[@]}
  fi

  # To make sure that the final time statistics are printed
  # differently when compared to those printed in inner executions.
  TIMEFORMAT='Executed in %3lR'
}

time process

printf "\n${BOLD}SUMMARY:${NC}\n\n"
printf "OK: $ok_tests    "
printf "FAIL: $fail_tests    "
printf "ERROR: $error_tests    "
printf "NOT EXECUTED: $not_executed_tests    "
printf "Total: $total_tests\n"

