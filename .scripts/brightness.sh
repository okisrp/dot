#!/usr/bin/env sh

SHORT_OPTS="udl:s:"
LONG_OPTS="up,down,level:,set:"

OPTS="$(getopt --options "${SHORT_OPTS}" \
  --longoptions "${LONG_OPTS}" --alternative \
  --name "Adjust Brightness" -- "${@}")"

if [[ "${?}" != 0 ]]; then
  echo "Failed parsing options." >&2
  exit 1
fi

eval set -- "${OPTS}"

UP=true
DOWN=false
LEVEL=5
SET=0

while true; do
  case "${1}" in
    "-u" | "--up" )
      UP=true && DOWN=false; shift ;;
    "-d" | "--down" )
      DOWN=true && UP=false; shift ;;
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

CMD="$(which brightnessctl) --quiet"

if (( "${SET}" != 0 )); then
  sh -c "${CMD} set ${SET}%"
elif [[ "${DOWN}" = true ]]; then
  sh -c "${CMD} set ${LEVEL}-%"
else
  sh -c "${CMD} set +${LEVEL}%"
fi

sh -c "~/.scripts/dwm/status.sh -r"
