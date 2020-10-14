#!/bin/bash


RED='\033[0;31m'   	# RED
NC='\033[0m'       	# No Color
GREEN='\033[0;32m' 	# GREEN
YELLOW='\33[1;33m' 	# YELLOW
BLINK1='\e[5m'
BLINK2='\e[25m'	   	#BLINK
INV='\e[7m'         #INVERTED
LGREEN='\e[102m'
BOLD='\e[1m'
DEBUG="false"

test_prog () {
	MODE=$1
	TESTFILE=$2
	DEBUG=$3
	
	
	OCAMLRES=$(./main.native -mode ${MODE} -i ${TESTFILE} -mon ${MON} --parse)
	if [[ $DEBUG == "true" ]]
	then
	  echo "${OCAMLRES}"
	fi
	OCMLRES2=$( echo "${OCAMLRES}" | grep "MAIN return"  )
	cd ../JS2ECMA-SL
	filename=${TESTFILE%.*}
	JSRES=$(node src/parse_esl.js ../implementation/${filename}.json ${MON})
	if [[ $DEBUG == "true" ]]
	then
	  echo "{$JSRES}"
	fi
	JRES2=$( echo "${JSRES}" | grep "MAIN return"  )
	if [[ "${JRES2}" == "${OCMLRES2}" ]]
	then
		
		printf "${BOLD}${GREEN}${INV}OK!${NC} \n(  ${JRES2}  )"
	else
		printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC} \n(  ${JRES2}  )\n(  ${OCMLRES2}  )"
	fi
}

test_dir (){
	MODE=$1
	DEBUG=$2
	POSITIVEDIR=$3
	NEGATIVEDIR=$4


	
	echo "${POSITIVEDIR_aux}"
	for fullpath in $POSITIVEDIR/*.esl
	do
		# File identifier
		cd ../implementation
		fullpath_aux=${fullpath#"../implementation/"}
		IFS='/' read -r -a array <<< "$fullpath" 
		printf "${YELLOW}\n${array[3]}${NC} "
		echo "${fullpath_aux}"
		# Program run
		test_prog $MODE $fullpath_aux $DEBUG
		#MAINGREP=$(grep "MAIN return" <<< "${RESULT}")
		#IFS='->' read -r -a mainarray <<< "${MAINGREP}"	
		#if [[ "${mainarray[0]}" == "MAIN return " ]]
		#then
		#	printf "${GREEN}${INV}OK${NC} \t-> ${mainarray[2]}"
		#else
		#	monitor=`grep "MONITOR EXCEPTION" <<< "${RESULT}"`
		#	IFS='->' read -r -a monitor_array <<< "$monitor"
		#	printf "${RED}${BLINK1}${INV}FAIL${BLINK2}${NC}\t-> ${monitor_array[2]}"
		#fi 
	done
	#if [[ $3 != "" ]]
	#then
	#	printf "\n\n${BOLD}_____________ ILLEGAL FLOWS _____________${NC}"
#
#		for fullpath in $3/*.esl
#		do
#			# File identifier
#			IFS='/' read -r -a array <<< "$fullpath" 
#			printf "${YELLOW}\n${array[3]}${NC} "
#			
#			# Program run
#			RESULT=$(./main.native -i ${fullpath} -mode $1) 
#			MONGREP=$(grep "MONITOR EXCEPTION" <<< "${RESULT}")
#			IFS='->' read -r -a mainarray <<< "${MONGREP}"	
#			if [[ "${mainarray[0]}" == "MONITOR EXCEPTION " ]]
#			then
#				printf "${GREEN}${INV}OK${NC} \t-> ${mainarray[2]}"
#			else
#				printf "${RED}${BLINK1}${INV}FAIL${BLINK2}${NC}\t-> ${monitor_array[2]}"
#			fi 
#		done
#	fi
}
for arg in "$@"
do
	case $arg in
		-i)
		TESTFILE="$2"
		shift
		shift
		;;
		-pd)
		POSITIVEDIR="$2"
		shift
		shift
		;;
		-nd)
		NEGATIVEDIR="$2"
		shift
		shift
		;;
		-m)
		MODE="$2"
		shift
		shift
		;;
		-d|--debug)
		DEBUG="true"
		shift
		;;
		-mon)
		MON="$2"
		shift
		shift
		;;
	esac
done

printf "\t${RED}-------------------------------${NC}\n"
printf "\t\tJS Test Tool\n"
printf "\t${RED}-------------------------------${NC}\n"
if [[ ${TESTFILE} != "" ]]
then
	printf "${YELLOW}Test File:${NC}\t\t${TESTFILE}\n"
elif [[ ${POSITIVEDIR} != "" ]] 
	then
	printf "${YELLOW}Positive Test Directory:${NC}\t${POSITIVEDIR}\n"
	if [[ ${NEGATIVEDIR} != "" ]]
		then	
		printf "${YELLOW}Negative Test Directory:${NC}\t${NEGATIVEDIR}\n"
	fi
elif [[ ${NEGATIVEDIR} != "" ]]
	then	
	printf "${YELLOW}Negative Test Directory:${NC}\t${NEGATIVEDIR}\n"
fi
printf "${YELLOW}Interpretation Mode:${NC}\t\t$MODE\n"
printf "${YELLOW}DEBUG:${NC}\t\t\t\t${DEBUG}\t\n"
printf "${YELLOW}Monitor Mode:${NC}\t\t\t${MON}\n"



if [[ ${TESTFILE} != "" ]]
then
	cd ../implementation
	TESTFILE_aux=${TESTFILE#"../implementation/"}
	test_prog $MODE $TESTFILE_aux $DEBUG
elif [[ $POSITIVEDIR != "" && $NEGATIVEDIR != "" ]]
then
	printf "Testing Prositive Directories...\n"
	test_dir $MODE $DEBUG $POSITIVEDIR 
	printf "Testing Negative Directories...\n"
	test_dir $MODE $DEBUG $NEGATIVEDIR
elif [[ $POSITIVEDIR != "" ]]
then
	printf "Testing Prositive Directories...\n"
	test_dir $MODE $DEBUG $POSITIVEDIR 
elif [[ $NEGATIVEDIR != "" ]]
then
	printf "Testing Negative Directories...\n"
	test_dir $MODE $DEBUG $NEGATIVEDIR
else
	printf "Test files missing, use [-i], [-pt] and [-nt]"
fi

