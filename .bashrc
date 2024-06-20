export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"

export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"
export MPLAYER_HOME="${XDG_CONFIG_HOME}/mplayer"
export W3M_DIR="${XDG_DATA_HOME}/w3m"

export TERMINFO="${XDG_DATA_HOME}/terminfo"
export TERMINFO_DIRS="${XDG_DATA_HOME}/terminfo:/usr/share/terminfo"

export TERMINAL="$( which kitty )"
export BROWSER="$( which firefox )"

if [[ -d "${HOME}/.local/bin" ]]; then
	export PATH="${HOME}/.local/bin:$PATH"
fi

if [[ -x "$( command -v nvim )" ]]; then
	EDITOR="$( which nvim )"
	test -t 0 && alias e="${EDITOR}"
	export MANPAGER="${EDITOR} +Man!"
	export EDITOR
fi

if [[ -d "${HOME}/.config/emacs/bin" ]]; then
	export PATH="${XDG_CONFIG_HOME}/emacs/bin:$PATH"
	export DOOMDIR="${XDG_CONFIG_HOME}/doom"
fi

[[ ! -t 0 ]] && return

[[ "${DISPLAY}" ]] && shopt -s checkwinsize

test -d "${XDG_STATE_HOME}/less" || mkdir -p "${XDG_STATE_HOME}/less"
LESSHISTFILE="${XDG_STATE_HOME}/less/history"
export LESSHISTFILE

shopt -s histappend

export HISTCONTROL="ignoreboth"

test -d "${XDG_STATE_HOME}/bash" || mkdir -p "${XDG_STATE_HOME}/bash"
HISTFILE="${XDG_STATE_HOME}/bash/history"
HISTSIZE=2000

shopt -s autocd
shopt -s cdspell

bind "set completion-ignore-case on"

bind '"\ek": previous-history'
bind '"\ej": next-history'

export FZF_DEFAULT_OPTS="--bind=alt-j:down,alt-k:up \
	--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
	--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
	--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

PKGFILE="/usr/share/doc/pkgfile/command-not-found.bash"
[[ -r "${PKGFILE}" ]] && source "${PKGFILE}"

COMPFILE="/usr/share/bash_completion/bash_completion"
[[ -r "${COMPFILE}" ]] && source "${COMPFILE}"

unset PKGFILE COMPFILE

if [[ -x "$(command -v dircolors)" ]]; then
	DIRCOLORS="${XDG_CONFIG_HOME}/dircolors"
	test -r "${DIRCOLORS}" || dircolors -p > "${DIRCOLORS}"
	eval "$(dircolors -b "${DIRCOLORS}")"
	unset DIRCOLORS
fi

alias mv="$(which mv) -i"
alias cp="$(which cp) -i"

alias du="$(which du) -h"
alias df="$(which df) -h"
alias free="$(which free) -m"

alias grep="$(which grep) --color=auto"
alias egrep="$(which grep) --color=auto -E"
alias ip="$(which ip) -color=auto"

alias cal="$(which cal) -m"

test -x "$(command -v bat)" && alias cat="$(which bat)"

test -x "$(command -v wget)" && alias wget="$(which wget) --no-hsts"

if [[ -x "$(command -v eza)" ]]; then
	alias l="$(which eza) -lA --group-directories-first \
		--no-user --git --time-style \"+%y/%m/%d\""
else
	alias l="$(which ls) -gGAh --group-directories-first --color=auto"
fi

NC="\\[\\033[00m\\]"
RED="${NC}\\[\\033[00;31m\\]"
GRN="${NC}\\[\\033[00;32m\\]"
YLW="${NC}\\[\\033[00;33m\\]"
BLU="${NC}\\[\\033[00;34m\\]"
PUR="${NC}\\[\\033[00;35m\\]"

PGB="\$(git branch 2> /dev/null | sed -e \
	'/^[^*]/d' -e 's/* \(.*\)/ ${BLU}(${GRN}\1${BLU})/')"

PS1="${PUR}\u${RED}@${BLU}\h ${YLW}\W${NC}"
PS1+="${BLU}${PGB}${RED} > ${NC}"
export PS1

unset NC RED GRN YLW BLU PUR PGB

if [[ -x "$( command -v zoxide )" ]]; then
	eval "$( zoxide init bash )"
fi
