#!/usr/bin/env sh

while true; do
	xsetroot -name "$( sh -c "${HOME}/.scripts/dwm/status.sh -p" )"
	sleep 1m &

	echo $! > "${DWMSLEEPPID}"
	wait $!
done &

xrandr --size 1920x1080 &

xset r rate 350 55 &

xset s off -dpms &

unclutter --timeout 3 --start-hidden --jitter 16 &

xinput set-prop "Synaptics TM3336-001" "libinput Tapping Enabled" 1 &

SXHKDRC="${XDG_CONFIG_HOME}/sxhkd/sxhkdrc"
if [ -f "${SXHKDRC}" ]; then
	sxhkd -c "${SXHKDRC}" &
fi

setxkbmap -layout us,ua -option grp:shifts_toggle &

WALLPAPER="${XDG_DATA_HOME}/wallpapers/default"
if [ -e "${WALLPAPER}" ]; then
	feh --no-fehbg --bg-scale "${WALLPAPER}" &
fi

exec dwm
