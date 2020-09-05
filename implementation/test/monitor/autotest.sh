#!/bin/bash

echo "NSU Monitor Test"

# Color definition
RED='\033[0;31m'   # RED
NC='\033[0m'       # No Color
GREEN='\033[0;32m' # GREEN
YELLOW='\33[1;33m' # YELLOW

cd ../../

echo "_________ LEGAL FLOWS _________"

for fullpath in test/monitor/legal_flows/*.esl
do
	# File identifier
	IFS='/' read -r -a array <<< "$fullpath" 
	printf "${YELLOW}\n${array[3]}${NC} "\
	
	# Program run
	RESULT=`./main.native -i ${fullpath} -mode ci | grep "MAIN return"` 
	IFS='->' read -r -a array <<< "$RESULT"	
	if [[ "${array[0]}" == "MAIN return " ]]
	then
		printf "${GREEN}OK${NC} -> ${array[2]}"
	else
		printf "${RED}FAIL${NC}"
	fi 
done

#echo "_________ ILLEGAL FLOWS _________"
#for filename in test/monitor/illegal_flows/*.esl
#do 
#    printf "${YELLOW}\n${filename}\n${NC}"
#    ./main.native -i ${filename} -mode ci | tail -n 4

#done
