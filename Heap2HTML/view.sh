#! /bin/bash

# Bash color codes:

# Black        0;30     Dark Gray     1;30
# Red          0;31     Light Red     1;31
# Green        0;32     Light Green   1;32
# Brown/Orange 0;33     Yellow        1;33
# Blue         0;34     Light Blue    1;34
# Purple       0;35     Light Purple  1;35
# Cyan         0;36     Light Cyan    1;36
# Light Gray   0;37     White         1;37

RED='\033[0;31m'
LIGHTRED='\033[1;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color


# JSON file may have erroneous syntax, this function fixes it
findAndReplace() {
	echo -e "- Fixing syntax errors..."
	sed -i "s/ 'empty\b/ \"'empty\"/g" $1
	sed -i "s/ 'undefined\b/ \"'undefined\"/g" $1
	sed -i "s/ 'null\b/ \"'null\"/g" $1
	sed -i "s/ nan\b/ \"NaN\"/g" $1
	sed -i "s/ -inf\b/ \"-Infinity\"/g" $1
	sed -i "s/ inf\b/ \"Infinity\"/g" $1
	sed -i "s/(/[/g" $1
	sed -i "s/)/]/g" $1
	echo -e "Done."
}

# Convert JSON to HTML so we can view it with browser
convertToHTML() {
	echo -e "- Converting JSON to HTML..."
	node index.js -f $1
	echo -e "Done."
}

# Open HTML with preferred browser
openFile() {
	echo -e "- Opening HTML..."
	electron $1
	echo -e "Done."
}

# Check for errors...

if [ -z "$1" ] ;  then
  echo -e "${LIGHTRED}ERROR:${NC} Must supply the name of the heap file (.json)"
  exit
fi

if ! which node > /dev/null ; then
  echo -e "${LIGHTRED}ERROR:${NC} Could not find node, make sure that you have it installed and in your path."
  exit
fi

if ! which electron > /dev/null ; then
  echo -e "${LIGHTRED}ERROR:${NC} Could not find electron, make sure that you have it installed and in your path."
  exit
fi

# MAIN CODE

file=$1

echo -e "File: ${YELLOW}$file${NC}"

findAndReplace $file
convertToHTML $file
openFile heap.html


