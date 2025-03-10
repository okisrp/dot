if [[ -d "$HOME/.local/bin" ]]; then
	export PATH="$HOME/.local/bin:$PATH"
fi

export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"

export DF_SCRIPTS_DIR=$XDG_DATA_HOME/scripts

if [ -n "$BASH_VERSION" ]; then
	test -r "$HOME/.bashrc" && . "$HOME/.bashrc"
fi

export DWM_SLEEP_PID="${XDG_DATA_HOME}/dwm_sleep_pid"
test -f "$DWM_SLEEP_PID" && rm "$DWM_SLEEP_PID"

if [ "$(tty)" = "/dev/tty1" ]; then
	if [ -x "$XDG_CONFIG_HOME/sx/sxrc" ]; then
		exec ssh-agent sx
	fi
fi
