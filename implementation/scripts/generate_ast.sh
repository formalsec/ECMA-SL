#!/usr/bin/env bash
printf " -------------------------------\n"
printf " \tECMA-SL Generate ASTs Tool\n"
printf " -------------------------------\n\n"


function usage {
  echo -e "Usage: $(basename $0) [OPTION]... [-g]"
  echo -e '
  -g <dir>   Generates the ASTs for all the test files (.js) that are contained in the directory passed as argument.
             These ASTs are stored in the folder "generated_ASTs/".

  Options:
  -H         Include the harness file in the generated ASTs'
}


function createMain262JSFile() {
  # echo "3.1. Copy contents to temporary file"
  cat /dev/null > "output/main262_$now.js"
  if [[ $(echo -e "$2" | awk '/flags: \[onlyStrict\]/ {print $1}') != "" ]]; then
    echo "\"use strict\";" >> "output/main262_$now.js"
  fi
  if [ $WITH_HARNESS -eq 1 ]; then
    cat "test/test262/environment/harness.js" >> "output/main262_$now.js"
  fi
  cat "$1" >> "output/main262_$now.js"

  if [ $? -ne 0 ]; then
    exit 1
  fi
}


function generateAST() {
  local FILENAME=$1

  local initChars=${FILENAME:0:2}
  local output_esl=$OUTPUT_FOLDER
  if [ $WITH_HARNESS -eq 1 ]; then
    output_esl=$OUTPUT_FOLDER_WITH_HARNESS
  fi

  if [[ $initChars == "./" ]]; then
    output_esl+=${FILENAME:2}".esl"
  else
    output_esl+=$FILENAME".esl"
  fi

  METADATA=$(cat "$FILENAME" | awk '/\/\*---/,/---\*\//')

  # echo "3.1. Copy contents to temporary file"
  createMain262JSFile $FILENAME "$METADATA"

  # echo "3.2. Create the AST of the program in the file FILENAME and compile it to a \"Plus\" ECMA-SL program"
  JS2ECMASL=$(node ../JS2ECMA-SL/src/index.js --optimised -c -i output/main262_$now.js -o $output_esl 2>&1)

  if [[ "$JS2ECMASL" != "The file has been saved!" ]]; then
    printf "$FILENAME ... ${BOLD}${RED}${INV}ERROR${NC}\n"

    echo -e "$JS2ECMASL"
    exit 2
  fi
}


function generateASTs() {
  local initial_folder=$1

  # Create folders if don't exist
  local all_dirs=($(find $initial_folder -type d))

  for dir in "${all_dirs[@]}"; do
    if [ $WITH_HARNESS -eq 1 ]; then
      if [ ! -d "$OUTPUT_FOLDER_WITH_HARNESS$dir" ]; then
        mkdir -p "$OUTPUT_FOLDER_WITH_HARNESS$dir"
      fi
    else
      if [ ! -d "$OUTPUT_FOLDER$dir" ]; then
        mkdir -p "$OUTPUT_FOLDER$dir"
      fi
    fi
  done

  # process files
  local all_files=($(find $initial_folder -type f -name '*.js'))

  for file in "${all_files[@]}"; do
    printf "."
    generateAST $file
  done
}


#
# BEGIN
#
function exit_abnormal() {
  usage
  exit 1
}

[ ! -f "test/test262/environment/harness.js" ] && (echo -e "Missing harness file. (Expecting it in \"test/test262/environment/harness.js\")"; exit 1)

declare -i WITH_HARNESS=0
declare now=$(date +%y%m%dT%H%M%S)
declare OUTPUT_FOLDER="generated_ASTs/"
declare OUTPUT_FOLDER_WITH_HARNESS=$OUTPUT_FOLDER"with_harness/"


# If called with no arguments ...
if [[ ${#} -eq 0 ]]; then
   usage
   exit 1
fi

# Check that "output" directory exists and, if not, create it
if [ ! -d "output" ]; then
  mkdir "output"
fi

optstring=":Hhg:"
while getopts ${optstring} arg; do
  case $arg in
    H) WITH_HARNESS=1 ;;
    g) time generateASTs "$OPTARG" ;;
    h) usage ;;

    :)
      echo -e "Error: -${OPTARG} requires an argument.\n"
      exit_abnormal
      ;;
    *)
      echo -e "Error: Invalid option: -${OPTARG}\n"
      exit_abnormal
  esac
done
