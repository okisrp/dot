export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"

if [[ -d "${HOME}/.local/bin" ]]; then
	export PATH="${HOME}/.local/bin:$PATH"
fi

if [[ -x "$(command -v nvim)" ]]; then
	export EDITOR="$(which nvim)"
	export MANPAGER=""$(which nvim)" +Man!"
fi

[[ ! -t 0 ]] && return

[[ "${DISPLAY}" ]] && shopt -s checkwinsize

shopt -s autocd

bind "set completion-ignore-case on"

bind '"\ek": previous-history'
bind '"\ej": next-history'
bind '"\ea": kill-whole-line'

HISTFILE="${XDG_STATE_HOME}/bash/history"
LESSHISTFILE="${XDG_STATE_HOME}/less/history"

if [[ -r "/usr/share/doc/pkgfile/command-not-found.bash" ]]; then
	source "/usr/share/doc/pkgfile/command-not-found.bash"
fi

if [[ -r "/usr/share/bash_completion/bash_completion" ]]; then
	source "/usr/share/bash_completion/bash_completion"
fi

if [[ -x "$(command -v dircolors)" ]]; then
	test -r "${HOME}/.dircolors" || dircolors -p > "${HOME}/.dircolors"
	eval "$(dircolors -b "${HOME}/.dircolors")"
fi

test -x "$(command -v nvim)" && alias e="$(which nvim)"

alias mv="$(which mv) -i"
alias cp="$(which cp) -i"

alias du="$(which du) -h"
alias df="$(which df) -h"
alias free="$(which free) -m"

alias grep="$(which grep) --color=auto"
alias egrep="$(which grep) --color=auto -E"
alias ip="$(which ip) -color=auto"

alias cal="$(which cal) -m"

alias pwd="$(which pwd) -P"

test -x "$(command -v bat)" && alias cat="$(which bat)"

test -x "$(command -v wget)" && alias wget="$(which wget) --no-hsts"

if [[ -x "$(command -v lsd)" ]]; then
	alias l="$(which lsd) --group-directories-first --icon never \
		--blocks permission,size,date,name -lA --date '+%y/%m/%d'"
else
	alias l="$(which ls) -gGAh --group-directories-first --color=auto"
fi

RST="\\[\\033[00m\\]"
RED="${RST}\\[\\033[00;31m\\]"
GRN="${RST}\\[\\033[00;32m\\]"
YLW="${RST}\\[\\033[00;33m\\]"
BLU="${RST}\\[\\033[00;34m\\]"
PUR="${RST}\\[\\033[00;35m\\]"

PGB="\$(git branch 2> /dev/null | sed -e \
	'/^[^*]/d' -e 's/* \(.*\)/ ${BLU}(${GRN}\1${BLU})/')"

PS1="${BLU}${PUR}\u${RED}@${BLU}\h ${YLW}\W${RST}"
PS1="${PS1}${BLU}${PGB}${RED} > ${RST}"

unset RST RED GRN YLW BLU PUR PGB

if [[ -x "$(command -v zoxide)" ]];then
	eval "$(zoxide init bash)"
fi
