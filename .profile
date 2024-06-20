if [ -n "${BASH_VERSION}" ]; then
	test -r "${HOME}/.bashrc" && . "${HOME}/.bashrc"
fi

[ "$( tty )" = "/dev/tty1" ] && startx
