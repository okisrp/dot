#!/usr/bin/env sh

#
#  ________  ___       __   _____ ______
# |\   ___ \|\  \     |\  \|\   _ \  _   \
# \ \  \_|\ \ \  \    \ \  \ \  \\\__\ \  \
#  \ \  \ \\ \ \  \  __\ \  \ \  \\|__| \  \
#   \ \  \_\\ \ \  \|\__\_\  \ \  \    \ \  \
#    \ \_______\ \____________\ \__\    \ \__\
#     \|_______|\|____________|\|__|     \|__|
#

#
# DWM Installation Script
# by Oleksii Kapula
#

OPTS="$( getopt --options "ixs:" \
	--longoptions "install,xorg,sltools:" \
	--alternative --name "Manage Volume" -- "${@}" )"

if [[ "${?}" != 0 ]]; then
	echo "Failed parsing options." >&2
	exit 1
fi

eval set -- "${OPTS}"

INSTALL=false
XORG=false
SLTOOLS=""

while true; do
	case "${1}" in
		"-i" | "--install" )
			INSTALL=true ; shift ;;
		"-x" | "--xorg" )
			XORG=true ; shift ;;
		"-s" | "--sltools" )
			SLTOOLS="${2}" ; shift 2 ;;
		"--" )
			shift ; break ;;
		* )
			break ;;
	esac
done

DEPS=(
	"base-devel"
	"git"
	"libx11"
	"libxft"
	"libxinerama"
	"feh"
	"brightnessctl"
	"sxhkd"
	"unclutter"
	"pipewire"
	"pipewire-alsa"
	"pipewire-pulse"
	"pipewire-jack"
	"wireplumber"
	"alsa-utils"
	"playerctl"
	"calc"
)

XDEPS=(
	"server"
	"xinit"
	"xprop"
	"xsetroot"
	"xset"
	"xinput"
	"setxkbmap"
	"xev"
)

if [[ "${INSTALL}" = true ]]; then
	read -p "Type \`yes' in upper case to proceed: " PROCEED
	[[ ! "${PROCEED}" = "YES" ]] && exit
else
	exit
fi

if [[ "${XORG}" = true ]]; then
	DEPS+=( "xorg" )
else
	for DEP in "${XDEPS[@]}"; do
		DEPS+=( "xorg-${DEP}" )
	done
fi

if [[ ! -x "$( command -v yay )" ]]; then
	sudo pacman -S --needed base-devel git
	git clone --depth 1 https://aur.archlinux.org/yay-bin.git
	cd yay-bin/ && makepkg -si
	cd - && rm yay-bin/ -rf
fi

yay -S --needed "${DEPS[@]}"

[[ -z "${SLTOOLS}" ]] && exit

SLTOOLSDIR="${HOME}/.local/state/sltools"

[[ -d "${SLTOOLSDIR}" ]] || mkdir -p "${SLTOOLSDIR}"

IFS=","
read -ra TOOLS <<< "${SLTOOLS}"
SLTOOLS=( "${TOOLS[@]}" )
unset TOOLS

for TOOL in "${SLTOOLS[@]}"; do
	git clone --depth 1 "https://git.suckless.org/${TOOL}" "${SLTOOLSDIR}/${TOOL}"
done
