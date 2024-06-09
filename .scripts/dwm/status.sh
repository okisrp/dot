#!/usr/bin/env sh

OPTS="$(getopt --options "pr" --longoptions "print,refresh" \
	--alternative --name "DWM Status Bar" -- "${@}")"

if [[ "${?}" != 0 ]]; then
	echo -n "Failed parsing options." >&2
	exit 1
fi

eval set -- "${OPTS}"

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
RST="^c#cdd6f4^"

BAT() {
	CAP="$(cat /sys/class/power_supply/BAT0/capacity)"
	STAT="$(cat /sys/class/power_supply/BAT0/status)"

	ICON="󰂃"
	[[ "${STAT}" = "Charging" ]] && ICON="󰂄"

	if (( "${CAP}" >= 60 )); then
		CLR="${GRN}"
	elif (( "${CAP}" < 60 )) && (( "${CAP}" >= 30 )); then
		CLR="${YLW}"
	else
		CLR="${RED}"
	fi

	printf "%s%s %s%s%%" "${CLR}" "${ICON}" "${RST}" "${CAP}"
}

BRT() {
	BRT="$(calc "100 * $(brightnessctl get) / $(brightnessctl max)")"
	BRT="$(printf "%.*f" 0 "${BRT:2:-1}")"

	if (( "${BRT}" >= 50 )); then
		CLR="${BLU}"
	elif (( "${BRT}" < 50 )) && (( "${BRT}" >= 20 )); then
		CLR="${YLW}"
	else
		CLR="${RED}"
	fi

	printf "%s%s %s%s%%" "${CLR}" "󱠂" "${RST}" "${BRT}"
}

DATE() {
	DATE="$(date '+%b %d %a')"
	TIME="$(date '+%R')"

	printf "%s %s" "${DATE}" "${TIME}"
}

WLAN() {
	case "$(cat /sys/class/net/wl*/operstate 2> /dev/null)" in
		"up" )
			STAT="${BLU}󱚽 ${RST}Up" ;;
		"down" )
			STAT="${RED}󱚼 ${RST}Dn" ;;
	esac

	printf "%s" "${STAT}"
}

LYT() {
	KEY="$(xset -q | grep "LED" | awk "{ print \$10 }")"

	LABEL="us" && CLR="${BLU}"
	[[ "${KEY}" = "00001000" ]] && LABEL="ua" && CLR="${RED}"

	printf "%s%s %s%s" "${CLR}" "󰌑" "${RST}"  "${LABEL}"
}

VOL() {
	MUTE="$(pactl get-sink-mute @DEFAULT_SINK@)"
	VOL="$(pactl get-sink-volume @DEFAULT_SINK@ \
		| grep "Volume:" | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')"

	ICON="󱄡" ; CLR="${GRN}"
	if [[ "${MUTE}" = "Mute: yes" ]]; then
		ICON="󰸈" ; CLR="${RED}"
	elif [[ "${VOL}" = "0" ]]; then
		CLR="${YLW}"
	fi

	printf "%s%s %s%s%%" "${CLR}" "${ICON}" "${RST}" "${VOL}"
}

if [[ "${PRINT}" = true ]]; then
	DEL="󰿟"
	printf "%s" " $(LYT) ${DEL} $(VOL) ${DEL} $(BRT) ${DEL} $(BAT) ${DEL} $(WLAN) ${DEL} $(DATE) "
else
	PID="$( pstree -p | grep -E 'dwm.*sh.*sleep' | sed -r 's/.*\(([0-9]*).*/\1/g' )"
	[[ ! -z "${PID}" ]] && kill "${PID}"
fi
