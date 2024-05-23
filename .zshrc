#!/usr/bin/env bash

##############################################################################
# Brew Autocomplete

if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

##############################################################################
# ZSH config

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="agnoster"

plugins=(
  git
  zsh-syntax-highlighting
  aws
  ssh-agent
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
command -v nvim &> /dev/null
if [ $? -ne 0 ]; then
  command -v vim &> /dev/null
  if [ $? -ne 0 ]; then
    command -v vi &> /dev/null
      if [ $? -ne 0 ]; then
        echo 'vim/vi not installed!'
      else
        export EDITOR='vi'
      fi
    else
      export EDITOR='vim'
  fi
else
  export EDITOR='nvim'
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

##############################################################################
# GIT

command -v git &> /dev/null
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

command -v svn &> /dev/null
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

  # cd into the root directory of the repo
  svn-root() {
    set -o pipefail
    root_dir=$(svn info | grep "Root Path:" | sed -rn "s/^.*\:\s(.*)$/\1/p")
    if [ $? -eq 0 ]; then
      cd $root_dir
    fi
  }
fi

##############################################################################
# NVM

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

##############################################################################
# RBENV

RBENVDIR="$HOME/.rbenv/bin"
if [ -d $RBENVDIR ]; then
  export PATH="$RBENVDIR:$PATH"
  eval "$(rbenv init -)"
fi

##############################################################################
# Install Ruby Gems to ~/gems

export GEM_HOME="$HOME/gems"
export PATH="$HOME/gems/bin:$PATH"

###############################################################################
# PHP 5.6 & PHPBREW

PHPDIR='/usr/local/opt/php@5.6'
if [ -d "$PHPDIR/bin" -a -d "$PHPDIR/sbin" ]; then
  export PATH="$PHPDIR/bin:$PHPDIR/sbin:$PATH"
fi

[[ -e ~/.phpbrew/bashrc ]] && source ~/.phpbrew/bashrc

###############################################################################
# thefuck/fuck

command -v thefuck &> /dev/null
if [ $? -eq 0 ]; then
  eval $(thefuck --alias)
  alias fuck='fuck --yeah'
fi

###############################################################################
# Rust / Cargo

[[ -e ~/.cargo/env ]] && source ~/.cargo/env

###############################################################################
# Python
export PATH="$HOME/.local/bin:$PATH"

###############################################################################
# dotnet
#
export PATH="$PATH:$HOME/.dotnet/tools"

###############################################################################
# nim
#
export PATH="$PATH:$HOME/.nimble/bin"

[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

###############################################################################
# Expand !! and immediately execute rather than verify and hit enter
#
unsetopt histverify

###############################################################################
# zoxide (replaces cd and adds cdi)
#
command -v zoxide &> /dev/null
if [ $? -eq 0 ]; then
  eval "$(zoxide init --cmd cd zsh)"
fi
