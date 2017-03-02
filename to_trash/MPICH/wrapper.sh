#!/bin/sh

pattern="${1}"
shift
output=$(eval "$* 2>&1")

if echo "$output" | grep -ie "$pattern"; 
then
    echo "$output"
    exit 0
else
    if test "${?}" -ne 0;  
    then
        echo "$output"
        exit 1
    else
        echo "$output"
        exit 0
    fi  
fi

