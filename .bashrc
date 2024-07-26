export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"

export DWM_SLEEP_PID="${XDG_DATA_HOME}/dwm_sleep_pid"

export INPUTRC="${XDG_CONFIG_HOME}/readline/inputrc"
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"
export MPLAYER_HOME="${XDG_CONFIG_HOME}/mplayer"
export W3M_DIR="${XDG_DATA_HOME}/w3m"

export TERMINFO="${XDG_DATA_HOME}/terminfo"
export TERMINFO_DIRS="${XDG_DATA_HOME}/terminfo:/usr/share/terminfo"

if [[ -x "$( command -v kitty )" ]]; then
	TERMINAL="$( which kitty )"
	export TERMINAL
fi

if type -P firefox &> /dev/null; then
	BROWSER="$( type -P firefox )"
	export BROWSER
fi

if type -P nvim &> /dev/null; then
	EDITOR="$( which nvim )"
	test -t 0 && alias e="${EDITOR}"
	export MANPAGER="${EDITOR} +Man!"
	export EDITOR
fi

if [[ -d "${HOME}/.local/bin" ]]; then
	export PATH="${HOME}/.local/bin:$PATH"
fi

if [[ -d "${XDG_CONFIG_HOME}/emacs/bin" ]]; then
	export PATH="${XDG_CONFIG_HOME}/emacs/bin:$PATH"
	export DOOMDIR="${XDG_CONFIG_HOME}/doom"
fi

[[ ! -t 0 ]] && return

[[ "${DISPLAY}" ]] && shopt -s checkwinsize

NC='\[\033[00m\]'
RED='\[\033[00;31m\]'
GRN='\[\033[00;32m\]'
YLW='\[\033[00;33m\]'
BLU='\[\033[00;34m\]'
PUR='\[\033[00;35m\]'

PS1="${PUR}\u${RED}@${BLU}\h ${YLW}\w ${RED}\$ ${NC}"
export PS1

unset NC RED GRN YLW BLU PUR

shopt -s globstar

shopt -s autocd
shopt -s cdspell
shopt -s dirspell

shopt -s no_empty_cmd_completion

shopt -s checkjobs

shopt -s histappend
shopt -s cmdhist

export HISTFILE="${XDG_STATE_HOME}/bashhist"
export HISTCONTROL="ignoreboth"
export HISTSIZE=2000

export LESSHISTFILE="${XDG_STATE_HOME}/lesshist"

export FZF_DEFAULT_OPTS="--bind=alt-j:down,alt-k:up \
	--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
	--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
	--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

if [[ -e "/usr/share/doc/pkgfile/command-not-found.bash" ]]; then
	source "/usr/share/doc/pkgfile/command-not-found.bash"
fi

if [[ -e "/usr/share/bash-completion/bash_completion" ]]; then
	source "/usr/share/bash-completion/bash_completion"
fi

if [[ -x "$( command -v dircolors )" ]]; then
	if [[ ! -e "${XDG_CONFIG_HOME}/dircolors" ]]; then
		dircolors -p > "${XDG_CONFIG_HOME}/dircolors"
	fi
	eval "$( dircolors -b "${XDG_CONFIG_HOME}/dircolors" )"
fi

alias mv="$( which mv ) -i"
alias cp="$( which cp ) -i"

alias du="$( which du ) -h"
alias df="$( which df ) -h"
alias free="$( which free ) -m"

alias grep="$( which grep ) --color=auto"
alias egrep="$( which grep ) --color=auto -E"
alias ip="$( which ip ) -color=auto"

alias cal="$( which cal ) -m"

test -x "$( command -v bat )" && alias cat="$( which bat )"

test -x "$( command -v wget )" && alias wget="$( which wget ) --no-hsts"

if [[ -x "$( command -v eza )" ]]; then
	alias l="$( which eza ) -lA --group-directories-first \
		--no-user --git --time-style \"+%y/%m/%d\""
else
	alias l="$( which ls ) -gGAh --group-directories-first --color=auto"
fi

if type -P emacs &> /dev/null; then
	alias emacs="$( type -P emacsclient ) -tca \"$( type -P emacs ) -nw\""
fi

if type -P ani-cli &> /dev/null; then
	alias ani-cli="$( which ani-cli ) --dub"
fi

if [[ -x "$( command -v zoxide )" ]]; then
	eval "$( zoxide init bash )"
fi
