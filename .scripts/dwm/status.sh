#!/usr/bin/env bash

SHORTOPTS="pr"
LONGOPTS="print,refresh"

OPTS="$( getopt -ao "${SHORTOPTS}" -l "${LONGOPTS}" -- "${@}" )"

if [[ "${?}" != 0 ]]; then
	echo -n "Failed parsing options." >&2
	exit 1
fi

eval set -- "${OPTS}"

unset SHORTOPTS LONGOPTS OPTS

PRINT=true
REFRESH=false

while true; do
	case "${1}" in
		"-p" | "--print" )
			PRINT=true ; REFRESH=false ; shift ;;
		"-r" | "--refresh" )
			PRINT=false ; REFRESH=true ; shift ;;
		"--" )
			shift ; break ;;
		* )
			break ;;
	esac
done

RED="^c#d20f39^"
YLW="^c#df8e1d^"
GRN="^c#40a02b^"
BLU="^c#1e66f5^"
NC="^c#cdd6f4^"

BAT0() {
	CAP="$(cat /sys/class/power_supply/BAT0/capacity)"
	STAT="$(cat /sys/class/power_supply/BAT0/status)"

	ICON="󱟤"
	[[ "${STAT}" = "Charging" ]] && ICON="󰂄"

	if (( "${CAP}" >= 60 )); then
		CLR="${GRN}"
	elif (( "${CAP}" < 60 )) && (( "${CAP}" >= 30 )); then
		CLR="${YLW}"
	else
		CLR="${RED}"
	fi

	printf "%s%s %s%s%%" "${CLR}" "${ICON}" "${NC}" "${CAP}"
}

BRIGHTNESS() {
	BRT="$( calc -d "100 * $( brightnessctl get ) / $( brightnessctl max )" )"
	BRT="$( printf "%.0f" "$( echo "scale=2;${BRT}" | bc )" )"

	if (( "${BRT}" >= 50 )); then
		CLR="${BLU}"
	elif (( "${BRT}" < 50 )) && (( "${BRT}" >= 20 )); then
		CLR="${YLW}"
	else
		CLR="${RED}"
	fi

	printf "%s%s %s%s%%" "${CLR}" "󱠂" "${NC}" "${BRT}"
}

DATETIME() {
	DATE="$( date '+%b %d %a' )"
	TIME="$( date '+%R' )"

	printf "%s %s" "${DATE}" "${TIME}"
}

WLAN() {
	case "$( cat /sys/class/net/wl*/operstate 2> /dev/null )" in
		"up" )
			STAT="${BLU}󱚽 ${NC}Up" ;;
		"down" )
			STAT="${RED}󱚼 ${NC}Dn" ;;
	esac

	printf "%s" "${STAT}"
}

VOLUME() {
	MUTE="$( pactl get-sink-mute @DEFAULT_SINK@ )"
	VOL="$( pactl get-sink-volume @DEFAULT_SINK@ \
		| grep "Volume:" | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,' )"

	ICON="󰕾" ; CLR="${GRN}"
	if [[ "${MUTE}" = "Mute: yes" ]]; then
		ICON="󰖁" ; CLR="${RED}"
	elif [[ "${VOL}" = "0" ]]; then
		CLR="${YLW}"
	fi

	printf "%s%s %s%s%%" "${CLR}" "${ICON}" "${NC}" "${VOL}"
}

if [[ "${PRINT}" = true ]]; then
	DL1="[ "
	DL2=" ]"
	printf " %s %s %s %s %s " "${DL1}$( VOLUME )${DL2}" \
		"${DL1}$( BRIGHTNESS )${DL2}" \
		"${DL1}$( BAT0 )${DL2}" \
		"${DL1}$( WLAN )${DL2}" \
		"${DL1}$( DATETIME )${DL2}"
elif [[ "${REFRESH}" = true ]]; then
	PID="$( cat "${XDG_DATA_HOME}/dwmsleeppid" | tr -d '\n' )"
	kill -kill $PID
fi
