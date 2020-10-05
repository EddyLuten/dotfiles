#!/usr/bin/env bash

source ./common.sh

mkdir -p ~/.fonts && cd ~/.fonts

# Source Serif Pro
git clone https://github.com/adobe-fonts/source-serif-pro.git
cp ~/.fonts/source-serif-pro/TTF/*.ttf ~/.fonts
rm -rf ~/.fonts/source-serif-pro

fc-cache -f -v
