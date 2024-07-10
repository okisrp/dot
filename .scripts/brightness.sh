#!/usr/bin/env bash

OPTS="$( getopt -ao "udl:s:" -l "up,down,level:,set:" -- "${@}" )"

[[ "${?}" != 0 ]] && exit 1

eval set -- "${OPTS}"
unset OPTS

UP=true
DOWN=false
LEVEL=5
SET=0

while true; do
	case "${1}" in
		"-u" | "--up" )
			UP=true
			DOWN=false
			shift
			;;
		"-d" | "--down" )
			DOWN=true
			UP=false
			shift
			;;
		"-l" | "--level" )
			LEVEL="${2}"
			shift 2
			;;
		"-s" | "--set" )
			SET="${2}"
			shift 2
			;;
		"--" )
			shift
			break
			;;
		* )
			break
			;;
	esac
done

type -P brightnessctl &> /dev/null || exit 1

CMD="$( which brightnessctl ) --quiet"

if (( "${SET}" != 0 )); then
	sh -c "${CMD} set ${SET}%"
elif [[ "${UP}" = true ]]; then
	sh -c "${CMD} set +${LEVEL}%"
elif [[ "${DOWN}" = true ]]; then
	sh -c "${CMD} set ${LEVEL}-%"
fi

if test -x "${HOME}/.scripts/dwm/status.sh"; then
	sh -c "${HOME}/.scripts/dwm/status.sh -r"
fi
