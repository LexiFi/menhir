#!/bin/bash

test (){
    out=$(./calc "calc0$1.in" 2>&1)
    ans=$(cat "calc0$1.exp")
    if [[ $out != $ans ]];
    then
        echo "Test failed $1"
    fi
}

for i in 0 1 2 3 4 5 6 7; do
    test "$i"
done