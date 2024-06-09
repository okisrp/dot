#!/usr/bin/env sh

OPTS="$(getopt --options "f:u:" --longoptions "format:,url:" \
	--alternative --name "YT CLI Downloader" -- "${@}")"

if [[ "${?}" != 0 ]]; then
	echo "Failed parsing options." >&2
	exit 1
fi

eval set -- "${OPTS}"

FORMAT="opus"
URL=""

[[ -x "$(command -v yt-dlp)" ]] || exit 1

while true; do
	case "${1}" in
		"-f" | "--format" )
			FORMAT="${2}"; shift 2 ;;
		"-u" | "--url" )
			URL="${2}"; shift 2 ;;
		"--" )
			shift; break ;;
		* )
			break ;;
	esac
done

if [[ -z "${URL}" ]]; then
	URL="$(xclip -selection clipboard -out)"
fi

yt-dlp --extract-audio --audio-format "${FORMAT}" \
	--audio-quality 0 "${URL}" --no-playlist
