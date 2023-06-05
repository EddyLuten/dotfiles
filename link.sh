#!/usr/bin/env bash

source ./common.sh

# Retrieve the directory in which this file lives
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

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
link_for .xmodmap
link_for .hyper.js

which code > /dev/null
if [ $? -eq 0 ]; then
    echo '-'
    echo 'Found vscode:  starting to ensure existence of directories.'
    code
    link_for '.config/Code/User/keybindings.json'
    link_for '.config/Code/User/settings.json'
    echo '-'
    if prompt 'Install recommended extensions?'; then
        cat ./.config/Code/User/extensions.list | xargs -L1 code --install-extension
    fi
fi

echo 'Done.'
