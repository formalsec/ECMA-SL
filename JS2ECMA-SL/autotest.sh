#!/bin/bash
printf "\t-------------------------------\n"
printf "\t\tJS Test Tool\n"
printf "\t-------------------------------\n"
printf "Test File:\t\test/basic2.esl\n"

cd ../implementation
./main.native -mode ci -i test/basic2.esl
cd ../JS2ECMA-SL

node src/parse_esl.js ../implementation/test/basic2.json