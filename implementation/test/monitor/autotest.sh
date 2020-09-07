#!/bin/bash

printf "\tNSU Monitor Test\n"

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


cd ../../

printf "${BOLD}__________ LEGAL FLOWS __________${NC}"

for fullpath in test/monitor/legal_flows/*.esl
do
	# File identifier
	IFS='/' read -r -a array <<< "$fullpath" 
	printf "${YELLOW}\n${array[3]}${NC} "
	
	# Program run
	RESULT=$(./main.native -i ${fullpath} -mode ci) 
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
printf "\n${BOLD}_________ ILLEGAL FLOWS _________${NC}"

for fullpath in test/monitor/illegal_flows/*.esl
do
	# File identifier
	IFS='/' read -r -a array <<< "$fullpath" 
	printf "${YELLOW}\n${array[3]}${NC} "
	
	# Program run
	RESULT=$(./main.native -i ${fullpath} -mode ci) 
	MONGREP=$(grep "MONITOR EXCEPTION" <<< "${RESULT}")
	IFS='->' read -r -a mainarray <<< "${MONGREP}"	
	if [[ "${mainarray[0]}" == "MONITOR EXCEPTION " ]]
	then
		printf "${GREEN}${INV}OK${NC} \t-> ${mainarray[2]}"
	else
		printf "${RED}${BLINK1}${INV}FAIL${BLINK2}${NC}\t-> ${monitor_array[2]}"
	fi 
done

#echo "_________ ILLEGAL FLOWS _________"
#for filename in test/monitor/illegal_flows/*.esl
#do 
#    printf "${YELLOW}\n${filename}\n${NC}"
#    ./main.native -i ${filename} -mode ci | tail -n 4

#done
