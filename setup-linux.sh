#!/usr/bin/env bash

source ./common.sh
CODENAME="$(cat /etc/lsb-release | sed -E -n 's/DISTRIB_CODENAME=(.+)/\1/p')"

wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
sudo install -o root -g root -m 644 packages.microsoft.gpg /etc/apt/trusted.gpg.d/
sudo sh -c 'echo "deb [arch=amd64 signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'
rm packages.microsoft.gpg

sudo apt update && sudo apt upgrade -y && sudo apt install apt-transport-https -y

sudo apt install -y\
    vim\
    curl\
    wget\
    git\
    zsh\
    keychain\
    fonts-firacode\
    fonts-powerline\
    fonts-inconsolata\
    fonts-dejavu\
    fonts-dejavu-core\
    fonts-dejavu-extra\
    pandoc\
    gnome-tweaks\
    python3\
    python3-pip\
    python3-dev\
    python3-setuptools\
    keepassxc\
    code &&\
sudo pip3 install thefuck

which upgrade_oh_my_zsh > /dev/null
if [ $? -ne 0 ]; then
  sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
  zsh -c "git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"
fi

which google-chrome > /dev/null
if [ $? -ne 0 ]; then
    wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb &&\
        sudo apt install ./google-chrome-stable_current_amd64.deb &&\
        rm -f google-chrome-stable_current_amd64.deb
fi

which snap > /dev/null
if [ $? -eq 0 ]; then
  sudo snap install code --classic
  sudo snap install emacs --classic
else
  echo "Snap is not installed, installing..."
  sudo apt install snapd
  echo "Installed snap, reboot the system and rerun this script!"
  exit
fi

which rustc > /dev/null
if [ $? -ne 0 ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

if prompt 'Link dotfiles now?'; then
    ./link.sh
fi

if prompt 'Run cleanup now?'; then
    ./cleanup.sh
fi

if prompt 'Configure GNOME with sane defaults?' then
    ./config.sh
fi

