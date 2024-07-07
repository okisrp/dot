if [ -n "${BASH_VERSION}" ]; then
	if [ -e "${HOME}/.bashrc" ]; then
		. "${HOME}/.bashrc"
	fi
fi

if [ "$(tty)" = "/dev/tty1" ]; then
	if [ -x "${HOME}/.xinitrc" ]; then
		exec startx
	fi
fi
