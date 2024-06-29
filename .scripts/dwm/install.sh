#!/usr/bin/env bash

SHORTOPTS="hixt:"
LONGOPTS="help,install,xorg,sltools:"

OPTS="$( getopt -ao "${SHORTOPTS}" -l "${LONGOPTS}" -- "${@}" )"

if [[ "${?}" != 0 ]]; then
	echo "Failed parsing options!" >&2
	exit 1
fi

eval set -- "${OPTS}"
unset SHORTOPTS LONGOPTS OPTS

HELP() {
	echo "See source code for help."
}

INSTALL=false
XORG=false

SLTOOLSFN() {
	local IFS=","
	read -ra SLTOOLS <<< "${1}"
}

while true; do
	case "${1}" in
		"-h" | "--help" )
			HELP
			shift ; exit 0 ;;
		"-i" | "--install" )
			INSTALL=true
			shift ; continue ;;
		"-x" | "--xorg" )
			XORG=true
			shift ; continue ;;
		"-t" | "--sltools" )
			SLTOOLSFN "${2}"
			shift 2 ; continue ;;
		"--" )
			shift ; break ;;
		* )
			echo "Internal error." >&2
			exit 1 ;;
	esac
done

if [[ "${INSTALL}" = true ]]; then
	CONFIRM="YES"
	printf "Type \033[0;32m%s\033[0m to proceed: " "${CONFIRM}"
	read -r PROCEED
	[[ "${PROCEED}" = "${CONFIRM}" ]] || exit 1
	unset CONFIRM PROCEED
else
	echo "In order to run the script pass -i option."
	exit 1
fi

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
	"bc"
	"terminus-font"
	"xclip"
)

XORGFN() {
	local XDEPS=(
		"server"
		"xinit"
		"xprop"
		"xsetroot"
		"xset"
		"xinput"
		"setxkbmap"
		"xev"
	)

	if [[ "${XORG}" = true ]]; then
		DEPS+=( "xorg" "xorg-xev" )
	else
		for DEP in "${XDEPS[@]}"; do
			DEPS+=( "xorg-${DEP}" )
		done
	fi
}

if [[ ! -x "$( command -v yay )" ]]; then
	sudo pacman -S --needed base-devel git
	git clone --depth 1 "https://aur.archlinux.org/yay-bin.git"
	cd yay-bin/ && makepkg -si
	cd - && rm yay-bin/ -rf
fi

XORGFN && yay -S --needed "${DEPS[@]}"

if [[ -n "${SLTOOLS[*]}" ]]; then
	SLTOOLSDIR="${HOME}/.local/state/sltools"
	[[ -d "${SLTOOLSDIR}" ]] || mkdir -p "${SLTOOLSDIR}"

	for TOOL in "${SLTOOLS[@]}"; do
		TOOLDIR="${SLTOOLSDIR}/${TOOL}"
		[[ -d "${TOOLDIR}" ]] && continue
		git clone --depth 1 "https://git.suckless.org/${TOOL}" "${TOOLDIR}"
		cd "${TOOLDIR}" && rm .git/ -rf ; cd -
	done
	unset TOOLDIR
fi

echo "All done successfully."
exit 0
