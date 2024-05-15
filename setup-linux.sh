#!/usr/bin/env bash
source ./common.sh
source /etc/os-release

# =============================================================================
# VSCode
sudo apt-get install wget gpg
wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
sudo install -D -o root -g root -m 644 packages.microsoft.gpg /etc/apt/keyrings/packages.microsoft.gpg
sudo sh -c 'echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/keyrings/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" > /etc/apt/sources.list.d/vscode.list'
rm -f packages.microsoft.gpg

# =============================================================================
# DOTNET (and other MS packages)
# wget https://packages.microsoft.com/config/$ID/$VERSION_ID/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
# sudo dpkg -i packages-microsoft-prod.deb
# rm packages-microsoft-prod.deb
# no longer needed post ubuntu 24

# =============================================================================
# Typora
wget -qO - https://typora.io/linux/public-key.asc | sudo tee /etc/apt/trusted.gpg.d/typora.asc
sudo add-apt-repository 'deb https://typora.io/linux ./'

# =============================================================================
# OBS
# sudo add-apt-repository ppa:obsproject/obs-studio

# =============================================================================
# Update apt and install packages
sudo apt update && sudo apt upgrade -y && sudo apt install apt-transport-https -y

sudo apt install -y\
  atop\
  code\
  curl\
  dotnet-runtime-8.0\
  dotnet-sdk-8.0\
  ffmpeg\
  firefox\
  flameshot\
  fonts-dejavu-core\
  fonts-dejavu-extra\
  fonts-dejavu\
  fonts-firacode\
  fonts-inconsolata\
  fonts-powerline\
  gcc\
  git\
  glslang-dev\
  glslang-tools\
  gnome-tweaks\
  htop\
  keepassxc\
  libev-dev\
  libx11-dev\
  libxi-dev\
  make\
  neovim\
  pandoc\
  pipx\
  pkg-config\
  python3-dev\
  python3-pip\
  python3-setuptools\
  python3\
  texlive-fonts-extra\
  texlive-fonts-recommended\
  texlive\
  typora\
  ubuntu-restricted-extras\
  wget\
  zsh\

# =============================================================================
# Crystal
curl -fsSL https://crystal-lang.org/install.sh | sudo bash

# =============================================================================
# Snaps

sudo snap install emacs --classic
sudo snap install obsidian --classic
sudo snap install blender --classic
sudo snap install\
  discord\
  gimp\
  krita\
  spotify\

# =============================================================================
# Python packages

pipx install \
  gnome-extensions-cli\
  markdown-word-count\
  mdx_truly_sane_lists\
  mkdocs-alias-plugin\
  mkdocs-awesome-pages-plugin\
  mkdocs-categories-plugin\
  mkdocs-live-edit-plugin\
  mkdocs-material\
  mkdocs\
  neoteroi-mkdocs\

# =============================================================================
# zoxide
curl -sS https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | bash

# =============================================================================
# wezterm
curl -fsSL https://apt.fury.io/wez/gpg.key | sudo gpg --yes --dearmor -o /usr/share/keyrings/wezterm-fury.gpg
echo 'deb [signed-by=/usr/share/keyrings/wezterm-fury.gpg] https://apt.fury.io/wez/ * *' | sudo tee /etc/apt/sources.list.d/wezterm.list
sudo apt update && sudo apt install wezterm

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

which rustc > /dev/null
if [ $? -ne 0 ]; then
  curl https://sh.rustup.rs -sSf | sh -s -- -y
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

if prompt 'Install gnome extensions?'; then
  gext install tiling-assistant@leleat-on-github && gext enable tiling-assistant@leleat-on-github
  gext install tophat@fflewddur.github.io && gext enable tophat@fflewddur.github.io
  gext install emoji-copy@felipeftn && gext enable emoji-copy@felipeftn
  gext install caffeine@patapon.info && gext enable caffeine@patapon.info
fi

if prompt 'Install middle-mouse paste blocking for Linux?'; then
  pushd .
  cd $HOME && git clone https://github.com/milaq/XMousePasteBlock.git
  cd XMousePasteBlock
  make
  sudo make install
  popd
  mkdir -p $HOME/.config/autostart
  cp xmousepasteblock.desktop $HOME/.config/autostart
fi
