#!/usr/bin/env bash

# Retrieve the directory in which this file lives
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

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

backup_check() {
    if [ -f "$HOME/$1" ]; then
        if prompt "$HOME/$1 exists, back it up as $HOME/$1.bak?"; then
            mv "$HOME/$1" "$HOME/$1.bak"
        else
            if prompt "Delete $HOME/$1 instead of backing it up?"; then
                rm "$HOME/$1"
            else
                echo "Skipping $1 because the file exists..."
                return 1
            fi
        fi
    fi
    return 0
}

link_for() {
    echo '-'
    if ! prompt "Link $1?"; then return 0; fi
    if ! backup_check "$1"; then return 1; fi
    ln -s "$DIR/$1" "$HOME/$1"
    if [ $? -eq 0 ]; then
        echo "Successfully linked $1"
    else
        echo "Linking $1 failed!"
    fi
}

link_for .zshrc
link_for .aliases
link_for .emacs
link_for .vimrc
