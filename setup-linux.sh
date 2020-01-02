#!/usr/bin/env bash

source ./common.sh
CODENAME="$(cat /etc/lsb-release | sed -E -n 's/DISTRIB_CODENAME=(.+)/\1/p')"

sudo apt update && sudo apt upgrade -y

# This needs to be a bit less janky for ubuntu and mint...
hostnamectl | grep -i ubuntu
if [ $? -eq 0 ]; then
  sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys ACCAF35C
  sudo sh -c "echo 'deb http://apt.insynchq.com/ubuntu $CODENAME non-free contrib' > /etc/apt/sources.list.d/insync.list"
  sudo apt install insync
fi

hostnamectl | grep -i linuxmint
if [ $? -eq 0 ]; then
  sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys ACCAF35C
  sudo sh -c "echo 'deb http://apt.insynchq.com/mint $CODENAME non-free contrib' > /etc/apt/sources.list.d/insync.list"
  sudo apt install insync
fi

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
    thefuck\
    gnome-tweaks\
    python3\
    python3-pip\
    python3-dev\
    python3-setuptools\
    telegram-desktop &&\
sudo pip3 install thefuck

which upgrade_oh_my_zsh > /dev/null
if [ $? -ne 0 ]; then
  sh -c "$(wget -O- https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
  zsh -c "git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"
fi

which google-chrome > /dev/null
if [ $? -ne 0 ]; then
    wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb &&\
        sudo dpkg -i google-chrome-stable_current_amd64.deb
    [[ -e google-chrome-stable_current_amd64.deb ]] && rm -f google-stable_current_amd64.deb
fi

which snap > /dev/null
if [ $? -eq 0 ]; then
  sudo snap install code --classic
  sudo snap install emacs --classic
  sudo snap install \
    spotify\
    keepassxc
else
  echo "Snap is not installed, installing..."
  sudo apt install snapd
  echo "Installed snap, reboot the system and rerun this script!"
  exit
fi

which calibre > /dev/null
if [ $? -ne 0 ]; then
  sudo -v && wget -nv -O- https://download.calibre-ebook.com/linux-installer.sh | sudo sh /dev/stdin
fi

which rustc > /dev/null
if [ $? -ne 0 ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

if prompt 'Link dotfiles now?'; then
    ./link.sh
fi
