if [ -n "${BASH_VERSION}" ]; then
	if [ -e "${HOME}/.bashrc" ]; then
		. "${HOME}/.bashrc"

		if [ "$( tty )" = "/dev/tty1" ]; then
			if [ -x "${XDG_CONFIG_HOME}/sx/sxrc" ]; then
				exec ssh-agent sx
			fi
		fi
	fi
fi
