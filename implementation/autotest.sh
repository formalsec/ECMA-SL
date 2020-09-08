#!/bin/bash
printf "\t-------------------------------\n"
printf "\t\tECMA-SL Test Tool\n"
printf "\t-------------------------------\n"
printf "Monitor Mode:\t\t$1\n"
printf "(OK) Test Directory:\t$2\n"
if [[ $3 != "" ]]
then
	printf "(FAIL) Test Directory\t$3\n"
fi
printf "\t-------------------------------\n\n"
# Color definition
RED='\033[0;31m'   	# RED
NC='\033[0m'       	# No Color
GREEN='\033[0;32m' 	# GREEN
YELLOW='\33[1;33m' 	# YELLOW
BLINK1='\e[5m'
BLINK2='\e[25m'	   	#BLINK
INV='\e[7m'         #INVERTED
LGREEN='\e[102m'
BOLD='\e[1m'


printf "${BOLD}______________ LEGAL FLOWS ______________${NC}"

for fullpath in $2/*.esl
do
	# File identifier
	IFS='/' read -r -a array <<< "$fullpath" 
	printf "${YELLOW}\n${array[3]}${NC} "
	
	# Program run
	RESULT=$(./main.native -i ${fullpath} -mode $1) 
	MAINGREP=$(grep "MAIN return" <<< "${RESULT}")
	IFS='->' read -r -a mainarray <<< "${MAINGREP}"	
	if [[ "${mainarray[0]}" == "MAIN return " ]]
	then
		printf "${GREEN}${INV}OK${NC} \t-> ${mainarray[2]}"
	else
		monitor=`grep "MONITOR EXCEPTION" <<< "${RESULT}"`
		IFS='->' read -r -a monitor_array <<< "$monitor"
		printf "${RED}${BLINK1}${INV}FAIL${BLINK2}${NC}\t-> ${monitor_array[2]}"
	fi 
done
if [[ $3 != "" ]]
then
	printf "\n\n${BOLD}_____________ ILLEGAL FLOWS _____________${NC}"

	for fullpath in $3/*.esl
	do
		# File identifier
		IFS='/' read -r -a array <<< "$fullpath" 
		printf "${YELLOW}\n${array[3]}${NC} "
		
		# Program run
		RESULT=$(./main.native -i ${fullpath} -mode $1) 
		MONGREP=$(grep "MONITOR EXCEPTION" <<< "${RESULT}")
		IFS='->' read -r -a mainarray <<< "${MONGREP}"	
		if [[ "${mainarray[0]}" == "MONITOR EXCEPTION " ]]
		then
			printf "${GREEN}${INV}OK${NC} \t-> ${mainarray[2]}"
		else
			printf "${RED}${BLINK1}${INV}FAIL${BLINK2}${NC}\t-> ${monitor_array[2]}"
		fi 
	done
fi

