#!/usr/bin/env bash

source ./common.sh
CODENAME="$(cat /etc/lsb-release | sed -E -n 's/DISTRIB_CODENAME=(.+)/\1/p')"

wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
sudo install -o root -g root -m 644 packages.microsoft.gpg /etc/apt/trusted.gpg.d/
sudo sh -c 'echo "deb [arch=amd64 signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'
rm packages.microsoft.gpg

wget -qO - https://typora.io/linux/public-key.asc | sudo tee /etc/apt/trusted.gpg.d/typora.asc
sudo add-apt-repository 'deb https://typora.io/linux ./'

sudo apt update && sudo apt upgrade -y && sudo apt install apt-transport-https -y

sudo apt install -y\
  neovim\
  curl\
  wget\
  git\
  zsh\
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
  code\
  typora\
  texlive\
  texlive-fonts-recommended\
  texlive-fonts-extra\

pip3 install thefuck --user

which upgrade_oh_my_zsh > /dev/null
if [ $? -ne 0 ]; then
  sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
  zsh -c "git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"
fi

which nvm > /dev/null
if [ $? -ne 0 ]; then
  wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
fi

which google-chrome > /dev/null
if [ $? -ne 0 ]; then
  wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb &&\
    sudo apt install ./google-chrome-stable_current_amd64.deb &&\
    rm -f google-chrome-stable_current_amd64.deb
fi

which snap > /dev/null
if [ $? -eq 0 ]; then
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

if prompt 'Configure GNOME with sane defaults?'; then
  ./config.sh
fi

if prompt 'Install Fonts?'; then
  ./fonts.sh
fi

if prompt 'Install middle-mouse paste blocking for Linux?'; then
  pushd .
  sudo apt install libev-dev libx11-dev libxi-dev
  cd ~ && git clone https://github.com/milaq/XMousePasteBlock.git
  cd XmousePaseBlock
  make
  sudo make install
  popd
  mkdir -p ~/.config/autostart
  cp xmousepasteblock.desktop ~/.config/autostart
fi

