#!/usr/bin/env bash

source ./common.sh

mkdir -p ~/.fonts && cd ~/.fonts

# JetBrains Mono
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/JetBrains/JetBrainsMono/master/install_manual.sh)"

fc-cache -f -v

