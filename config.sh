#!/usr/bin/env bash

source ./common.sh

# Use sed to strip any non-guid characters
profile_id=`dconf read /org/gnome/terminal/legacy/profiles:/default | sed 's/[^a-z0-9-]//g'`

dconf write /org/gnome/desktop/interface/enable-animations false
dconf write /org/gnome/desktop/session/idle-delay "uint32 0"
dconf write /org/gnome/desktop/interface/gtk-theme "'Yaru-dark'"
dconf write /org/gnome/desktop/input-sources/xkb-options "['ctrl:nocaps', 'altwin:swap_lalt_lwin']"
dconf write /org/gnome/desktop/calendar/show-weekdate true
dconf write /org/gnome/desktop/notifications/show-banners false
dconf write /org/gnome/shell/extensions/dash-to-dock/dash-max-icon-size 32
dconf write /org/gnome/desktop/background/picture-uri "''"
dconf write /org/gnome/desktop/background/primary-color "'#000000'"
dconf write /org/gnome/desktop/background/secondary-color "'#000000'"
dconf write /org/gnome/desktop/interface/gtk-enable-primary-paste false

dconf write /org/gnome/terminal/legacy/profiles:/:$profile_id/custom-command "'/usr/bin/zsh'"
dconf write /org/gnome/terminal/legacy/profiles:/:$profile_id/use-theme-colors false
dconf write /org/gnome/terminal/legacy/profiles:/:$profile_id/foreground-color "'rgb(131,148,150)'"
dconf write /org/gnome/terminal/legacy/profiles:/:$profile_id/background-color "'rgb(0,43,54)'"

