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
ERRORS=0
PROGS=0

complete_test () {
 MODE=$1
 TESTFILE=$2
 DEGUB=$3

##OCAML CI + mon (parse)
 OCAMLmonRes=$(./main.native -mode ci -i ${TESTFILE} -mon nsu --parse)
	if [[ $DEBUG == "true" ]]
	then
	  echo "${OCAMLmonRES}"
	fi
	OCAMLmonRes2=$( echo "${OCAMLmonRes}" | grep "MAIN return"  )
	if [[ $OCAMLmonRes2 == "" ]]
	then
		OCAMLmonRes2=$( echo "${OCAMLmonRes}" | grep "MONITOR EXCEPTION"  )
	fi

## OCAML Inline + CI
	OCAMLinlineRes=$(./main.native -i ${TESTFILE} -mode ic)
	if [[ $DEBUG == "true" ]]
	then
	  echo "${OCAMLinlineRes}"
	fi
	OCAMLinlineRes2=$( echo "${OCAMLinlineRes}" | grep "MAIN return"  )
	if [[ $OCAMLinlineRes2 == "" ]]
	then
		OCAMLinlineRes2=$( echo "${OCAMLinlineRES}" | grep "MONITOR EXCEPTION" | tail -1 )
	fi

## OCAML parse -> JS + mon 
	cd ../JS2ECMA-SL
	filename=${TESTFILE%.*}
	JSRES=$(node src/parse_esl.js ../implementation/${filename}.json ${MON})
	if [[ $DEBUG == "true" ]]
	then
	  echo "{$JSRES}"
	fi
	JRES2=$( echo "${JSRES}" | grep "MAIN return"  )
	if [[ $JRES2 == "" ]]
	then
		JRES2=$( echo "${JSRES}" | grep "MONITOR EXCEPTION"  )
	fi

## OCAML inline -> parse -> JS
	cd ../JS2ECMA-SL
	filename=${TESTFILE%.*}
	JSinlineRES=$(node src/parse_esl.js ../implementation/${filename}.json)
	if [[ $DEBUG == "true" ]]
	then
	  echo "{$JSinlineRES}"
	fi
	JSinlineRES2=$( echo "${JSinlineRES}" | grep "MAIN return"  )
	if [[ $JSinlineRES2 == "" ]]
	then
		JSinlineRES2=$( echo "${JSinlineRES}" | grep "MONITOR EXCEPTION"  )
	fi

	printf "Results:"
	printf "\tOCAML CI+mon =\t${OCAMLmonRes2}"
	printf "\tOCAML IC + CI =\t${OCAMLinlineRes2}"
	printf "\t(PARSE) JS+mon =\t${JRES2}"
	printf "\tIC (PARSE) + JS =\t${JSinlineRes2}"
	PROGS=$((PROGS+1))


}
test_OCAML_JS () {
	MODE=$1
	TESTFILE=$2
	DEBUG=$3
	
	
	OCAMLRES=$(./main.native -mode ${MODE} -i ${TESTFILE} -mon ${MON} --parse)
	if [[ $DEBUG == "true" ]]
	then
	  echo "${OCAMLRES}"
	fi
	OCMLRES2=$( echo "${OCAMLRES}" | grep "MAIN return"  )
	if [[ $OCMLRES2 == "" ]]
	then
		OCMLRES2=$( echo "${OCAMLRES}" | grep "MONITOR EXCEPTION"  )
	fi
	cd ../JS2ECMA-SL
	filename=${TESTFILE%.*}
	JSRES=$(node src/parse_esl.js ../implementation/${filename}.json ${MON})
	if [[ $DEBUG == "true" ]]
	then
	  echo "{$JSRES}"
	fi
	JRES2=$( echo "${JSRES}" | grep "MAIN return"  )
	if [[ $JRES2 == "" ]]
	then
		JRES2=$( echo "${JSRES}" | grep "MONITOR EXCEPTION"  )
	fi
	if [[ "${JRES2}" == "${OCMLRES2}" ]]
	then
		
		printf "${BOLD}${GREEN}${INV}OK!${NC} \n(  ${JRES2}  )"

	else
		printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC} \n(  ${JRES2}  )\n(  ${OCMLRES2}  )"
		ERRORS=$((ERRORS+1))
	fi
	PROGS=$((PROGS+1))
}

test_OCAML_inline () {
	TESTFILE=$1
	DEBUG=$2

	
	OCAMLRES=$(./main.native -mode ci -i ${TESTFILE} -mon nsu)
	if [[ $DEBUG == "true" ]]
	then
	  echo "${OCAMLRES}"
	fi
	OCAMLRES2=$( echo "${OCAMLRES}" | grep "MAIN return"  )
	if [[ $OCAMLRES2 == "" ]]
	then
		OCAMLRES2=$( echo "${OCAMLRES}" | grep "MONITOR EXCEPTION"  )
	fi

	OCAML_INL_RES=$(./main.native -i ${TESTFILE} -mode ic  )
	if [[ $DEBUG == "true" ]]
	then
	  echo "${OCAML_INL_RES}"
	fi
	OCAML_INL_RES2=$( echo "${OCAML_INL_RES}" | grep "MAIN return"  )
	if [[ $OCAML_INL_RES2 == "" ]]
	then
		OCAML_INL_RES2=$( echo "${OCAML_INL_RES}" | grep "MONITOR EXCEPTION" | tail -1 )
	fi
	if [[ "${OCAML_INL_RES2}" == "${OCAMLRES2}" ]]
	then
		
		printf "${BOLD}${GREEN}${INV}OK!${NC} \n(  ${OCAML_INL_RES2}  )"

	else
		printf "${BOLD}${RED}${BLINK1}${INV}FAIL${NC} \n(  ${OCAML_INL_RES2}  )\n(  ${OCAMLRES2}  )"
		ERRORS=$((ERRORS+1))
	fi
	PROGS=$((PROGS+1))
}

test_ic (){
	MODE=$1
	DEBUG=$2
	POSITIVEDIR=$3
	NEGATIVEDIR=$4

	echo "Testing Inlining Compiler..."
	for fullpath in $POSITIVEDIR/*.esl
	do
		# File identifier
		cd ../implementation
		fullpath_aux=${fullpath#"../implementation/"}
		IFS='/' read -r -a array <<< "$fullpath" 
		printf "${YELLOW}\n${array[3]}${NC} "
		echo "${fullpath_aux}"
		# Program run
		test_OCAML_inline $fullpath_aux $DEBUG
	done

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
		
			test_OCAML_JS $MODE $fullpath_aux $DEBUG
		
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
printf "\n\n"
echo "Found ${ERRORS} anomalies from ${PROGS} executions"
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
elif [[ $POSITIVEDIR != "" && $MODE = "ic" ]]
then
	printf "Testing Inlining Copiler Directories...\n"
	test_ic $MODE $DEBUG $POSITIVEDIR 	
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

