#!/usr/bin/env sh

red="^c#d20f39^"
ylw="^c#df8e1d^"
grn="^c#40a02b^"
blu="^c#1e66f5^"
rst="^c#cdd6f4^"

battery() {
  cap="$(cat /sys/class/power_supply/BAT0/capacity)"
  stat="$(cat /sys/class/power_supply/BAT0/status)"

  icon="󰂃"
  [[ "${stat}" = "Charging" ]] && icon="󰂄"

  if [[ $cap -ge 60 ]]; then
    clr="${grn}"
  elif [[ $cap -lt 60 ]] && [[ $cap -ge 30  ]]; then
    clr="${ylw}"
  else
    clr="${red}"
  fi

  printf "%s%s %s%s%%" "${clr}" "${icon}" "${rst}" "${cap}"
}

brightness() {
  brt="$(calc "100 * $(brightnessctl get) / $(brightnessctl max)")"
  brt="$(printf "%.*f" 0 "${brt:2:-1}")"

  if [[ $brt -ge 50 ]]; then
    clr="${blu}"
  elif [[ $brt -lt 50 ]] && [[ $brt -ge 20  ]]; then
    clr="${ylw}"
  else
    clr="${red}"
  fi

  printf "%s%s %s%s%%" "${clr}" "󱠂" "${rst}" "${brt}"
}

datetime() {
  date="$(date '+%b %d %a')"
  time="$(date '+%R')"

  printf "%s %s" "${date}" "${time}"
}

wlan() {
  case "$(cat /sys/class/net/wl*/operstate 2> /dev/null)" in
    up)
      stat="${blu}󱚽 ${rst}Up"
      ;;
    down)
      stat="${red}󱚼 ${rst}Dn"
      ;;
  esac

  printf "%s" "${stat}"
}

layout() {
  key="$(xset -q | grep "LED" | awk "{ print \$10 }")"

  label="us" && clr="${blu}"
  [[ "${key}" = "00001000" ]] && label="ua" && clr="${red}"

  printf "%s%s %s%s" "${clr}" "󰌑" "${rst}"  "${label}"
}

volume() {
  mute="$(pactl get-sink-mute @DEFAULT_SINK@)"
  vol="$(pactl get-sink-volume @DEFAULT_SINK@ \
    | grep "Volume:" | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')"

  icon="󱄡"; clr="${grn}"
  if [[ $mute = "Mute: yes" ]]; then
    icon="󰸈"; clr="${red}"
  elif [[ $vol = "0" ]]; then
   clr="${ylw}"
  fi

  printf "%s%s %s%s%%" "${clr}" "${icon}" "${rst}" "${vol}"
}

printf "%s" " $(layout) 󰿟 $(volume) 󰿟 $(brightness) 󰿟 $(battery) 󰿟 $(wlan) 󰿟 $(datetime) "
