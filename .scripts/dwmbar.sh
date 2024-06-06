#!/usr/bin/env sh

battery() {
  cap="$(cat /sys/class/power_supply/BAT0/capacity)"
  stat="$(cat /sys/class/power_supply/BAT0/status)"

  icon="󰂃"
  [[ "${stat}" = "Charging" ]] && icon="󰂄"

  printf "%s %s%%" "${icon}" "${cap}"
}

brightness() {
  brt="$(calc "100 * $(brightnessctl get) / $(brightnessctl max)")"
  brt="$(printf "%.*f" 0 "${brt:2:-1}")"

  printf "%s %s%%" "󱠂" "${brt}"
}

datetime() {
  date="$(date '+%b %d %a')"
  time="$(date '+%R')"

  printf "%s %s" "${date}" "${time}"
}

wlan() {
  case "$(cat /sys/class/net/wl*/operstate 2> /dev/null)" in
    up)
      stat="󱚽 Up"
      ;;
    down)
      stat="󱚼 Dn"
      ;;
  esac

  printf "%s" "${stat}"
}

layout() {
  key="$(xset -q | grep "LED" | awk "{ print \$10 }")"

  label="us"
  [[ "${key}" = "00001000" ]] && label="ua"

  printf "${prp}%s %s" "󰌑" "${label}"
}

volume() {
  mute="$(pactl get-sink-mute @DEFAULT_SINK@)"
  vol="$(pactl get-sink-volume @DEFAULT_SINK@ \
    | grep "Volume:" | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')"

  icon="󱄡"
  if [[ $mute = "Mute: yes" ]]; then
    icon="󰸈"
  fi

  printf "%s %s%%" "${icon}" "${vol}"
}

printf "%s" " $(layout) 󰿟 $(volume) 󰿟 $(brightness) 󰿟 $(battery) 󰿟 $(wlan) 󰿟 $(datetime) "
