#!/usr/bin/env bash

source ./common.sh

promptDelete() {
	which $1 > /dev/null
	if [  $? -eq 0 ] && prompt "$2"; then
		sudo apt autoremove --purge $3 -y
	fi
}

promptDelete libreoffice 'Remove LibreOffice?' libreoffice-core
promptDelete cheese 'Remove Cheese?' cheese
promptDelete remmina 'Remove Remmina?' remmina
promptDelete transmission-gtk 'Remove Transmission?' transmission-gtk
promptDelete gnome-sudoku 'Remove Sudoku?' gnome-sudoku
promptDelete shotwell 'Remove Shotwell?' shotwell
promptDelete rhythmbox 'Remove Rhythmbox?' rhythmbox
promptDelete thunderbird 'Remove Thunderbird?' thunderbird

if prompt 'Peform apt cleanup?'; then
	sudo apt autoremove && sudo apt clean
fi

