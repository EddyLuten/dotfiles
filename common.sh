#!/usr/bin/env bash

# The following two functions are just some sugar
command_failed() {
    [ $? -ne 0 ]
}
command_succeeded() {
    [ $? -eq 0 ]
}

# Takes a string as its argument to prompt the user y/n
prompt() {
    printf "$1 (y/n) "
    read USER_RESULT

    echo "$USER_RESULT" | grep -i "y" &> /dev/null
    if command_succeeded; then return 0; else return 1; fi
}
