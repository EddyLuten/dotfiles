# dotfiles

My dotfiles for:

* [ZSH](.zshrc)
* [Global aliases](.aliases)
* [Emacs](.emacs)
* [Vim](.vimrc)

Install by running [`setup-linux`](setup-linux.sh) from the repo's directory.

After running the linking script, log out and back in for the changes to take effect.

Included scripts:

* [setup-linux.sh](setup-linux.sh) - required applications setup
* [link.sh](link.sh) - links the dotfiles to the home directory
* [cleanup.sh](cleanup.sh) - prompts to remove bloatware
* [config.sh](config.sh) - configures GNOME with decent default settings
* [fonts.sh](fonts.sh) - installs fonts that can't be installed via apt

[MIT Licensed](LICENSE).
