if [ -n "${BASH_VERSION}" ]; then
	test -r "${HOME}/.bashrc" && . "${HOME}/.bashrc"
fi

if [ "$( tty )" = "/dev/tty1" ] && [ -z "${DISPLAY}" ]; then
	test -x "${HOME}/.xinitrc" && exec startx
fi
