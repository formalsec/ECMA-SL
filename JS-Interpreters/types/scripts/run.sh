#!/bin/bash

check_return() {
    if [ $? != 0 ]; then
        exit 1
    fi
}

if [ $# != 1 ] && [ $# != 2 ]; then
    echo "Usage $(basename $0) <test_name> [--surpress]"
    exit 1
fi

if [ ! -f "testsuit/tests/$1.js" ]; then
    echo "File not found: testsuit/tests/$1.js"
    exit 1
fi

if [ $# = 2 ] && [ "$2" = "--surpress" ]; then
    touch temp.out
    make bin/asm/$1.asm.cesl >> temp.out
    check_return
    ../../main.native -mode ci -i bin/asm/$1.asm.cesl >> temp.out
    grep 'PROGRAM PRINT:' temp.out
    rm temp.out
    check_return
else
    make bin/asm/$1.asm.cesl
    check_return
    ../../main.native -mode ci -i bin/asm/$1.asm.cesl
    check_return
fi

exit 0