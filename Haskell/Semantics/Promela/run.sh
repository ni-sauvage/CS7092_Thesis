#!/bin/bash
case $1 in 
    "make")
        cd "../../../Grammars/Pml"
        bnfc -d -m "Pml.cf" && make
        ret_code=$?
        if [ $ret_code != 0 ]; then
            echo "Error creating PML grammar. Is BNFC installed correctly?"
            exit 1
        fi
        cd -
    ;;
    "run")
        parse=$("../../../Grammars/Pml/Pml/Test" < $2)
        ret_code=$?
        if [ $ret_code -ne 0 ]; then
            echo "Error $parse while parsing Promela file: $2. Does it exist and has a BNFC executable been built?"
            exit 1
        fi
        ast=$(printf "%s" $parse)
        ret_code=$?
        if [ $ret_code -ne 0 ]; then
            echo "Error $ast while grepping BNFC result: $2"
            exit 1
        fi
        echo $ast
        exit 0
    ;;
esac