#!/usr/bin/env sh

SHORT_OPTS="udml:s:"
LONG_OPTS="up,down,mute,level:,set:"

OPTS="$(getopt --options "${SHORT_OPTS}" \
  --longoptions "${LONG_OPTS}" --alternative \
  --name "Manage Volume" -- "${@}")"

if [[ "${?}" != 0 ]]; then
  echo "Failed parsing options." >&2
  exit 1
fi

eval set -- "${OPTS}"

UP=true
DOWN=false
MUTE=false
LEVEL=4
SET=0

while true; do
  case "${1}" in
    "-u" | "--up" )
      UP=true && DOWN=false; shift ;;
    "-d" | "--down" )
      DOWN=true && UP=false; shift ;;
    "-m" | "--mute" )
      MUTE=true; shift ;;
    "-l" | "--level" )
      LEVEL="${2}"; shift 2 ;;
    "-s" | "--set" )
      SET="${2}"; shift 2 ;;
    "--" )
      shift; break ;;
    * )
      break ;;
  esac
done

CMD="$(which pactl)"

UNMUTE() {
  sh -c "${CMD} set-sink-mute 0 0"
}

if (( "${SET}" != 0 )); then
  sh -c "${CMD} set-sink-volume 0 ${SET}%" && UNMUTE
elif [[ "${MUTE}" = true ]]; then
  sh -c "${CMD} set-sink-mute 0 toggle"
elif [[ "${DOWN}" = true ]]; then
  sh -c "${CMD} set-sink-volume 0 -${LEVEL}%" && UNMUTE
else
  sh -c "${CMD} set-sink-volume 0 +${LEVEL}%" && UNMUTE
fi

sh -c "~/.scripts/dwm/status.sh -r"
