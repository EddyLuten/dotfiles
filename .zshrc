#!/usr/bin/env bash

##############################################################################
# ZSH config
# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="agnoster"

plugins=(
  git
  zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

##############################################################################
# User configuration

# Shared aliases
source $HOME/.aliases

# Hide the user@host bit in the prompt
export DEFAULT_USER="$USER"
prompt_context(){}

# You may need to manually set your language environment
export LANG=en_US.UTF-8

##############################################################################
# Default editor

which vim > /dev/null
if [ $? -ne 0 ]; then
  which vi > /dev/null
    if [ $? -ne 0 ]; then
      echo 'vim/vi not installed!'
    else
      export EDITOR='vi'
    fi
  else
    export EDITOR='vim'
fi

###############################################################################
# Emacs Magic
export ALTERNATE_EDITOR='emacs'

##############################################################################
# Profile editing foo

profile_reload() { source "$HOME/.zshrc" }
profile_edit() { $EDITOR "$HOME/.zshrc" }
profile_update() {
  cd "$HOME/dotfiles" && git pull
  profile_reload
}

##############################################################################
# SSH

if [ -f "$HOME/.ssh/id_rsa" ]; then
  export SSH_KEY_PATH="~/.ssh/rsa_id"
else
  echo 'SSH keys not imported'
fi

which keychain > /dev/null
if [ $? -eq 0 ]; then
  eval `keychain -q --eval --agents ssh id_rsa`
else
  echo 'keychain not installed!'
fi

##############################################################################
# GIT

which git > /dev/null
if [ $? -eq 0 ]; then
  git config --global user.name "Eddy Luten"
  git config --global user.email "eddyluten@gmail.com"
  git config --global core.editor vim
  git config --global color.ui true
  git config --global format.pretty "%Cred%h%C(white) - %Cblue%an: %C(white)%s %Cgreen(%cr)%Creset"
  git config --global core.autocrl input
  git config --global core.fileMode true

  # aliases
  git config --global alias.incoming "!(git fetch --quiet && git log --pretty=format:'%C(yellow)%h %C(white)- %C(red)%an %C(white)- %C(cyan)%d%Creset %s %C(white)- %ar%Creset' ..@{u})"
  git config --global alias.outgoing "!(git fetch --quiet && git log --pretty=format:'%C(yellow)%h %C(white)- %C(red)%an %C(white)- %C(cyan)%d%Creset %s %C(white)- %ar%Creset' @{u}..)"
  git config --global alias.dirty-branches '!(git branch --merged development | grep -v development | egrep -v "(development|master)")'
  git config --global alias.clean-branches '!(git branch --merged development | egrep -v "(development|master)" | xargs -n 1 git branch -d)'
  git config --global alias.push-branch '!(git rev-parse --abbrev-ref HEAD | xargs -J % git push --set-upstream origin %)'
  git config --global alias.last 'log -1 HEAD'
else
  echo 'git not installed!'
fi

###############################################################################
# SVN

which svn > /dev/null
if [ $? -eq 0 ]; then
  # equivalent-esque of 'git add .'
  svn-apply() {
    echo '-- add new'
    svn st | grep '^\?' | sed 's/?    //' | xargs svn add
    echo '-- rm old'
    svn st | grep '^\!' | sed 's/!    //' | xargs svn rm
    echo '-- status'
    svn st
  }
fi

##############################################################################
# NVM

export NVM_DIR="$HOME/.nvm"
if [ -s /usr/local/opt/nvm/nvm.sh ]; then
  . "/usr/local/opt/nvm/nvm.sh"
elif [ -s "$NVM_DIR/nvm.sh" ]; then
  . "$NVM_DIR/nvm.sh"
fi

##############################################################################
# RBENV

RBENVDIR="$HOME/.rbenv/bin"
if [ -d $RBENVDIR ]; then
  export PATH="$RBENVDIR:$PATH"
  eval "$(rbenv init -)"
fi

###############################################################################
# PHP 5.6 & PHPBREW

PHPDIR='/usr/local/opt/php@5.6'
if [ -d "$PHPDIR/bin" -a -d "$PHPDIR/sbin" ]; then
  export PATH="$PHPDIR/bin:$PHPDIR/sbin:$PATH"
fi

[[ -e ~/.phpbrew/bashrc ]] && source ~/.phpbrew/bashrc

###############################################################################
# thefuck/fuck

which thefuck > /dev/null
if [ $? -eq 0 ]; then
  eval $(thefuck --alias)
  alias fuck='fuck --yeah'
fi
