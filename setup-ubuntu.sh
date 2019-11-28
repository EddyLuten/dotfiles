#!/usr/bin/env bash

source ./common.sh
CODENAME="$(cat /etc/lsb-release | sed -E -n 's/DISTRIB_CODENAME=(.+)/\1/p')"

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys ACCAF35C
sudo sh -c "echo 'deb http://apt.insynchq.com/ubuntu $CODENAME non-free contrib' > /etc/apt/sources.list.d/insync.list"

sudo apt update &&\
sudo apt upgrade -y &&\
sudo apt install -y\
    curl\
    wget\
    git\
    zsh\
    keychain\
    fonts-firacode\
    fonts-powerline\
    insync\
    pandoc\
    thefuck\
    gnome-tweaks\
    python3\
    python3-pip\
    python3-dev\
    python3-setuptools\
    telegram-desktop\

sudo pip3 install thefuck

sh -c "$(wget -O- https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

zsh -c "git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"

which code > /dev/null
if [ $? -ne 0 ]; then
    wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb &&\
        sudo dpkg -i google-chrome-stable_current_amd64.deb
    [[ -e google-chrome-stable_current_amd64.deb ]] && rm -f google-stable_current_amd64.deb
fi

sudo snap install code --classic
sudo snap install \
    spotify\
    keepassxc

sudo -v && wget -nv -O- https://download.calibre-ebook.com/linux-installer.sh | sudo sh /dev/stdin

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

if prompt 'Link dotfiles now?'; then
    ./link.sh
fi
