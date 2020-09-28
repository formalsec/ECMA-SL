#!/bin/bash
printf "\t-------------------------------\n"
printf "\t\tJS Test Tool\n"
printf "\t-------------------------------\n"
printf "Test File:\t\test/basic2.esl\n"

RED='\033[0;31m'   	# RED
NC='\033[0m'       	# No Color
GREEN='\033[0;32m' 	# GREEN
YELLOW='\33[1;33m' 	# YELLOW
BLINK1='\e[5m'
BLINK2='\e[25m'	   	#BLINK
INV='\e[7m'         #INVERTED
LGREEN='\e[102m'
BOLD='\e[1m'

cd ../implementation
OCAMLRES=$(./main.native -mode ci -i test/basic2.esl)
if [[ $1 == "debug" ]]
then
  echo "${OCAMLRES}"
fi
OCMLRES2=$( echo "${OCAMLRES}" | grep "MAIN return"  )


cd ../JS2ECMA-SL

JSRES=$(node src/parse_esl.js ../implementation/test/basic2.json)

if [[ $1 == "debug" ]]
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