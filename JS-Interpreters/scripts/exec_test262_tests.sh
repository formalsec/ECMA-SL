#!/usr/bin/env bash
printf " -------------------------------\n"
printf " \tECMA-SL JS Test262 Tool\n"
printf " -------------------------------\n"

RED='\033[0;31m'   	# RED
NC='\033[0m'       	# No Color
GREEN='\033[0;32m' 	# GREEN
YELLOW='\033[1;33m' 	# YELLOW
BLINK1='\e[5m'
BLINK2='\e[25m'	   	#BLINK
INV='\e[7m'         #INVERTED
LGREEN='\e[102m'
BOLD='\e[1m'

function usage {
  echo -e "Usage: $(basename $0) [OPTION]... [-dfirp]"
  echo -e '
  -d <dir>   Directory containing test files.
             All the tests available in the directory are executed.
  -f <file>  File to test.
  -i <file>  File containing the list of files to test.
  -r <dir>   Directory containing test files and/or directories.
             If the directories contain other directories, all the tests available in those directories are also executed.
  -p <dir>   Same as "-r" but using the pre-generated ASTs for the test files available in the directory passed as argument.

  Options:
  -6         Uses reference interpreter created for version 6 of the standard (in folder "ES6_interpreter")
  -E         Enable logging to file the tests executed with errors. File is "errors.log"
  -F         Enable logging to file the failed tests. File is "failures.log"
  -O         Enable logging to file the passed tests. File is "oks.log"
  -H         Consider harness file during execution with pre-generated ASTs.
  -S         Skip OCaml compilation.'
  exit 1
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

  if [ $LOG_UNSUPPORTED -eq 1 ]; then
    cat /dev/null > $LOG_UNSUPPORTED_FILE
    for unsupported in "${log_unsupported_arr[@]}"; do
      echo "$unsupported" >> $LOG_UNSUPPORTED_FILE
    done
  fi
}

function checkConstraints() {
  FILENAME=$1

  METADATA=$2
  # in case it's a negative test, save its expected error name in a variable to be used later.
  neg=$(echo -e "$METADATA" | awk '/negative:/ {print $2}')

  # If NEGATIVE is the empty string, we may be in a case where the negative metadata is split in "phase" and "type":
  if [[ "$neg" == "" ]]; then
    neg=$(echo -e "$METADATA" | awk '/negative:/,/---\*\//' | awk '/type:/ { print $2 }')
  fi

  return $neg
}


function createMain262JSFile() {
  # echo "3.1. Copy contents to temporary file"
  cat /dev/null > "$OUTPUT_ROOT/main262_$3.js"
  # Check if test is to run in strict code mode.
  if [[ $(echo -e "$2" | grep -e 'flags:.*onlyStrict') != "" ]]; then
    echo "\"use strict\";" >> "$OUTPUT_ROOT/main262_$3.js"
  fi
  # Only include harness file if not using pre-compiled ASTs or
  # if using pre-compiled ASTs they do not include the harness
  if [[ $WITH_PRE_COMPILED -ne 1 || $WITH_HARNESS -ne 1 ]]; then
    cat "$TEST_ROOT/test262/environment/harness.js" >> "$OUTPUT_ROOT/main262_$3.js"
  fi
  cat "$1" >> "$OUTPUT_ROOT/main262_$3.js"

  if [ $? -ne 0 ]; then
    exit 1
  fi
}

function handleSingleFile() {
  test -f $1 \
    || (echo "Ignoring \"$1\". It's not a valid file." \
    && return)

  local test_result=""

  FILENAME=$1
  printf "Testing ${FILENAME} ... "

  METADATA=$(cat "$FILENAME" | awk '/\/\*---/,/---\*\//')

  local ID=$RANDOM

  local NEG=$(checkConstraints $FILENAME "$METADATA")

  if [ $WITH_PRE_COMPILED -eq 1 ]; then # Copy pre-compiled AST file contents to target file.
    if [ $WITH_HARNESS -eq 1 ]; then
      cp "$OUTPUT_FOLDER_WITH_HARNESS/$FILENAME.esl" "$OUTPUT_ROOT/test262_ast_$ID.esl"
    else
      cp "$OUTPUT_FOLDER/$FILENAME.esl" "$OUTPUT_ROOT/test262_ast_$ID.esl"
    fi
  else
    # Copy contents to temporary file
    createMain262JSFile $FILENAME "$METADATA" "$ID"

    # Create the AST of the program in the file FILENAME and compile it directly to \"Core\" ECMA-SL
    JS2ECMASL=$(time \
      (node ../JS2ECMA-SL/src/index.js \
        -c \
        -i "$OUTPUT_ROOT/main262_$ID.js" \
        -o "$OUTPUT_ROOT/test262_ast_$ID.esl") 2>&1 1>&1
    )

    ast_duration_str=$(echo "$JS2ECMASL" | sed '$!d')

    # The asterisk is to ignore the timing statistics appended by the "time" command
    if [[ "$JS2ECMASL" != "The file has been saved!"* ]]; then
      printf "${BOLD}${RED}${INV}ERROR${NC}\n"

      if [ $LOG_ERRORS -eq 1 ]; then
        log_errors_arr+=("$FILENAME")
      fi

      ERROR_MESSAGE=$(echo -e "$JS2ECMASL" | head -n 1)

      test_result="$FILENAME,**ERROR**,$ERROR_MESSAGE,$ast_duration_str, , "
      return
    fi
  fi

  # Create the Core file by concatenating the AST and Interpreter Core version files."
  cat "$OUTPUT_ROOT/test262_ast_$ID.esl" > "$OUTPUT_ROOT/core_$ID.esl"
  echo ";" >> "$OUTPUT_ROOT/core_$ID.esl"
  cat "$OUTPUT_ROOT/interpreter.esl" >> "$OUTPUT_ROOT/core_$ID.esl"

  # Evaluate program and write the computed heap to the file heap.json."
  local toggle_silent_mode=""
  [ $LOG_ENTIRE_EVAL_OUTPUT -eq 0 ] && toggle_silent_mode="-s"
  ECMASLCI=$(time \
    (timeout 100 \
      ECMA-SL \
        -mode ci \
        -i "$OUTPUT_ROOT/core_$ID.esl" $toggle_silent_mode) 2>&1 1>&1
  )

  local EXIT_CODE=$?

  duration_str=$(echo "$ECMASLCI" | sed '$!d')
  # Remove timing statistics appended by the "time" command
  ECMASLCI=$(echo "$ECMASLCI" | sed '$d')

  # In case of a negative test, we're expecting to have a failure in the interpretation
  if [ -n "$NEG" ]; then
    # the "print substr" part of the piped awk command is to remove the double-quotes present in the string we're extracting
    local completion_value=$(echo -e "$ECMASLCI" | tail -n 10 | awk -F ", " '/MAIN pc ->/ {print substr($3, 2, length($3)-2)}')
    if [[ $NEG == $completion_value ]]; then
      printf "${BOLD}${GREEN}${INV}OK!${NC}\n"

      if [ $LOG_OKS -eq 1 ]; then
        log_oks_arr+=("$FILENAME")
      fi

      test_result="$FILENAME,_OK_, ,$ast_duration_str,$plus2core_duration_str,$duration_str"
    else
      printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC}\n"

      if [ $LOG_FAILURES -eq 1 ]; then
        log_failures_arr+=("$FILENAME")
      fi

      test_result="$FILENAME,**FAIL**, $RESULT, $ast_duration_str, $plus2core_duration_str, $duration_str"
    fi
  elif [ $EXIT_CODE -eq 0 ]; then
    printf "${BOLD}${GREEN}${INV}OK!${NC}\n"

    if [ $LOG_OKS -eq 1 ]; then
      log_oks_arr+=("$FILENAME")
    fi

    test_result="$FILENAME, _OK_, ,$ast_duration_str, $plus2core_duration_str, $duration_str"
  elif [ $EXIT_CODE -eq 1 ]; then
    printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC}\n"

    if [ $LOG_FAILURES -eq 1 ]; then
      log_failures_arr+=("$FILENAME")
    fi

    test_result="$FILENAME, **FAIL**, $RESULT, $ast_duration_str, $plus2core_duration_str, $duration_str"
  elif [ $EXIT_CODE -eq 2 ]; then 
    printf "${BOLD}${RED}${INV}ERROR${NC}\n"

    if [ $LOG_ERRORS -eq 1 ]; then
      log_errors_arr+=("$FILENAME")
    fi

    ERROR_MESSAGE=$(echo -e "$ECMASLCI" | tail -n 1)

    test_result="$FILENAME, **ERROR**, $ERROR_MESSAGE, $ast_duration_str, $plus2core_duration_str, $duration_str"
  elif [ $EXIT_CODE -eq 3 ]; then 
    printf "${BOLD}${YELLOW}${INV}UNSUPPORTED${NC}\n"

    if [ $LOG_UNSUPPORTED -eq 1 ]; then
      log_unsupported_arr+=("$FILENAME")
    fi


    test_result="$FILENAME, **UNSUPPORTED**, $RESULT, $ast_duration_str, $plus2core_duration_str, $duration_str"
  fi

  local output_file="$OUTPUT_ROOT/$(echo "${FILENAME%.js}.out" | sed "s/^..//")"
  test -e $(dirname $output_file) || mkdir -p $(dirname $output_file)
  echo $test_result > $output_file

  rm $OUTPUT_ROOT/test262_ast_$ID.esl $OUTPUT_ROOT/core_$ID.esl $OUTPUT_ROOT/main262_$ID.js

  if [ $LOG_ENTIRE_EVAL_OUTPUT -eq 1 ]; then
    # Output of the execution is written to the file result.txt
    echo "Writing interpretation output to file..."
    echo "$ECMASLCI" > $OUTPUT_ROOT/result_$ID.txt
  fi
}


function testFiles() {
  local files=($@)

  parallel -j $JOBS handleSingleFile ::: "${files[@]}"
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
  params+=("OK | FAIL | ERROR | UNSUPPORTED | NOT EXECUTED | Total")
  params+=(":---: | :---: | :---: | :---: | :---: | :---:")
  params+=("$ok_tests | $fail_tests | $error_tests | $unsupported_tests | $not_executed_tests | $total_tests")
  params+=("### Individual results")
  params+=("File path | Result | Observations | Creation of the AST | Plus to Core | Interpretation")
  params+=("--- | :---: | :---: | :---: | :---: | :---:")
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
      # Test directory
      handleSingleDirectory $output_file $dir

      local output_dir="$OUTPUT_ROOT/$(echo $dir | sed "s/^..//")"
      local outputs=$(find $output_dir -type f -name "*.out")
      local count=$(find $output_dir -type f -name "*.out" | wc -l)
      if [[ $count -ne 0 ]]; then
        local params=()
        dir_ok_tests=$(grep "_OK_" -R $output_dir | wc -l)
        dir_fail_tests=$(grep "**FAIL**" -R $output_dir | wc -l)
        dir_error_tests=$(grep "**ERROR**" -R $output_dir | wc -l)
        dir_unsupported_tests=$(grep "**UNSUPPORTED**" -R $output_dir | wc -l)
        dir_not_executed_tests=0 # these are never incremented?
        dir_total_tests=$count
        params+=("## Summary of the tests executed in \"$dir\"")
        params+=("OK | FAIL | ERROR | UNSUPPORTED | NOT EXECUTED | Total")
        params+=(":---: | :---: | :---: | :---: | :---: | :---:")
        params+=("$dir_ok_tests | $dir_fail_tests | $dir_error_tests | $dir_unsupported_tests | $dir_not_executed_tests | $dir_total_tests")
        params+=("")
        params+=("### Individual results")
        params+=("File path | Result | Observations | Creation of the AST | Plus to Core | Interpretation")
        params+=("--- | :---: | :---: | :---: | :---: | :---:")
        for out in $outputs; do
          params+=("$(cat $out | sed "s/,/|/g")")
        done
        params+=("")

        writeToFile $output_file "${params[@]}"
      fi
    else
      echo "Ignoring \"$dir\". It's not a valid directory"
      continue
    fi
  done
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

function processFromPreCompiled() {
  local dirs=($@)
  RECURSIVE=1
  WITH_PRE_COMPILED=1

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
declare NOW=$(date +%y%m%dT%H%M%S); export NOW
declare duration_str=""
# Counters
# Counters used in the directories

declare JOBS=1; export JOBS
declare NEGATIVE=""
declare -a results=()
declare -a files_results=()

declare -i RECURSIVE=0; export RECURSIVE
declare -i WITH_PRE_COMPILED=0; export WITH_PRE_COMPILED
declare -i WITH_HARNESS=0; export WITH_HARNESS
declare    TEST_ROOT=../test; export TEST_ROOT
declare    OUTPUT_ROOT=output; export OUTPUT_ROOT
declare    OUTPUT_FOLDER="$TEST_ROOT"/generated_ASTs; export OUPUT_FOLDER
declare    OUTPUT_FOLDER_WITH_HARNESS="$OUTPUT_FOLDER"/with_harness; export OUTPUT_FOLDER_WITH_HARNESS
declare -r OUTPUT_FILE="$OUTPUT_ROOT"/logs/results_"$NOW".md; export OUTPUT_FILE

declare -i LOG_ENTIRE_EVAL_OUTPUT=0; export LOG_ENTIRE_EVAL_OUTPUT

declare -i ES6=0; export ES6

declare -i SKIP_COMPILATION=0; export SKIP_COMPILATION
declare -i LOG_ERRORS=0; export LOG_ERRORS
declare -i LOG_UNSUPPORTED=0; export LOG_UNSUPPORTED
declare -i LOG_FAILURES=0; export LOG_FAILURES
declare -i LOG_OKS=0; export LOG_OKS
declare -r LOG_ERRORS_FILE="$OUTPUT_ROOT/logs/errors_$NOW.log"; export LOG_ERRORS_FILE
declare -r LOG_UNSUPPORTED_FILE="$OUTPUT_ROOT/logs/unsupported_$NOW.log"; export LOG_UNSUPPORTED_FILE
declare -r LOG_FAILURES_FILE="$OUTPUT_ROOT/logs/failures_$NOW.log"; export LOG_FAILURES_FILE
declare -r LOG_OKS_FILE="$OUTPUT_ROOT/logs/oks_$NOW.log"; export LOG_OKS_FILE
declare -a log_errors_arr=()
declare -a log_unsupported_arr=()
declare -a log_failures_arr=()
declare -a log_ok_arr=()

# Check that "output" directory exists and, if not, create it
# Check that "logs" directory exists and, if not, create it
test -e "$OUTPUT_ROOT"/logs || mkdir -p "$OUTPUT_ROOT"/logs
test -e "$OUTPUT_ROOT"/test && rm -rf "$OUTPUT_ROOT"/test

# Empty the contents of the output file
cat /dev/null > $OUTPUT_FILE

# Set OPAM environment variables
eval `opam env`

function compile() {
  #echo "1. Compile the ECMA-SL language"
  (cd ../ECMA-SL \
    && dune build \
    && dune install) || exit 1

  #echo "2. Install JS2ECMA-SL dependencies"
  (cd ../JS2ECMA-SL && npm install) || exit 1

  cd ../JS-Interpreters

  echo ""
}

# Define list of arguments expected in the input
optstring=":6EUFOHSd:f:i:r:p:j:"

declare -a dDirs=() # Array that will contain the directories to use with the arg "-d"
declare -a fFiles=() # Array that will contain the files to use with the arg "-f"
declare -a iFiles=() # Array that will contain the files to use with the arg "-i"
declare -a rDirs=() # Array that will contain the directories to use with the arg "-r"
declare -a preCompiledDirs=() # Array that will contain the directories to use with the arg "-p"

while getopts ${optstring} arg; do
  case $arg in
    6) ES6=1 ;;
    E) LOG_ERRORS=1 ;;
    U) LOG_UNSUPPORTED=1 ;;
    F) LOG_FAILURES=1 ;;
    O) LOG_OKS=1 ;;
    H) WITH_HARNESS=1 ;;
    S) SKIP_COMPILATION=1 ;;
    d) dDirs+=("$OPTARG") ;;
    f) fFiles+=("$OPTARG") ;;
    i) iFiles+=("$OPTARG") ;;
    r) rDirs+=("$OPTARG") ;;
    p) preCompiledDirs+=("$OPTARG");;
    j) JOBS="$OPTARG";;

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


function process() {
  # Environment variable used by the "time" tool
  TIMEFORMAT='%3lR'


  #echo "3. Create the file that will be compiled from \"Main\" to \"Core\" in step 3.4."
  if [ $ES6 -eq 1 ]; then
    echo "GOING TO RUN ES6"
    sed '1d' "ES6_interpreter/main.esl" > "$OUTPUT_ROOT/main.esl"
  else
    sed '1d' "ES5_interpreter/main.esl" > "$OUTPUT_ROOT/main.esl"
  fi

  # Compile program written in \"Plus\" to \"Core\
  ECMASLC=$(time \
    (ECMA-SL \
      -mode c \
      -i "$OUTPUT_ROOT/main.esl" \
      -o "$OUTPUT_ROOT/interpreter.esl") 2>&1 1>/dev/null
  )

  EXIT_CODE=$?

  plus2core_duration_str=$(echo "$ECMASLC" | sed '$!d')

  if [ $EXIT_CODE -ne 0 ]; then
    printf "${BOLD}${RED}${INV}ERROR${NC} during compilation of the \"ES_interpreter/plus.esl\" file\n"

    echo -e "$ECMASLC" | sed '$d'

    exit 2
  fi


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

  if [ ${#preCompiledDirs[@]} -ne 0 ]; then
    processFromPreCompiled ${preCompiledDirs[@]}
  fi

  # To make sure that the final time statistics are printed
  # differently when compared to those printed in inner executions.
  TIMEFORMAT='Executed in %3lR'
}

export -f handleSingleFile \
  checkConstraints \
  logStatusToFiles \
  createMain262JSFile

time process

ok_tests=$(grep "_OK_" -R "$OUTPUT_ROOT/test" | wc -l)
fail_tests=$(grep "**FAIL**" -R "$OUTPUT_ROOT/test" | wc -l)
error_tests=$(grep "**ERROR**" -R "$OUTPUT_ROOT/test" | wc -l)
unsupported_tests=$(grep "**UNSUPPORTED**" -R "$OUTPUT_ROOT/test" | wc -l)
total_tests=$(find "$OUTPUT_ROOT/test" -type f -name "*.out" -size +2c | wc -l)

printf "\n${BOLD}SUMMARY:${NC}\n\n"
printf "OK: $ok_tests    "
printf "FAIL: $fail_tests    "
printf "ERROR: $error_tests    "
printf "UNSUPPORTED: $unsupported_tests    "
printf "Total: $total_tests\n"
