if [ -n "${BASH_VERSION}" ]; then
	test -r "${HOME}/.bashrc" && . "${HOME}/.bashrc"
fi

export DWM_SLEEP_PID="${XDG_DATA_HOME:-"${HOME}/.local/share"}/dwm_sleep_pid"

test -f "${DWM_SLEEP_PID}" && rm "${DWM_SLEEP_PID}"

if [ "$( tty )" = "/dev/tty1" ]; then
	if [ -x "${XDG_CONFIG_HOME}/sx/sxrc" ]; then
		exec ssh-agent sx
	fi
fi
