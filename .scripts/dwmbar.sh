#!/usr/bin/env sh

battery() {
	cap="$(cat /sys/class/power_supply/BAT0/capacity)"
	stat="$(cat /sys/class/power_supply/BAT0/status)"

	printf "%s %s%%" "BAT0" "${cap}"
}

brightness() {
	brt="$(calc "100 * $(brightnessctl get) / $(brightnessctl max)")"
	brt="$(printf "%.*f" 0 "${brt:2:-1}")"

	printf "%s %s%%" "BRT" "${brt}"
}

datetime() {
	date="$(date '+%b %d %a')"
	time="$(date '+%R')"

	printf "%s %s" "${date}" "${time}"
}

wlan() {
	case "$(cat /sys/class/net/wl*/operstate 2> /dev/null)" in
		up)
			stat="Up"
			;;
		down)
			stat="Dn"
			;;
	esac

	printf "%s %s" "WLAN" "${stat}"
}

layout() {
	key="$(xset -q | grep "LED" | awk "{ print \$10 }")"

	label="us"
	[[ "${key}" = "00001000" ]] && label="ua"

	printf "%s %s" "KLY" "${label}"
}

volume() {
	mute="$(pactl get-sink-mute @DEFAULT_SINK@)"
	vol="$(pactl get-sink-volume @DEFAULT_SINK@ \
		| grep "Volume:" | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')"

	label="VOL"
	if [[ $mute = "Mute: yes" ]]; then
		label="V01"
	fi

	printf "%s %s%%" "${label}" "${vol}"
}

printf "%s" " $(layout) / $(volume) / $(brightness) / $(battery) / $(wlan) / $(datetime) "
