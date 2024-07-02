#   ________  ________  ________  ___  ___  ________  ________
#  |\   __  \|\   __  \|\   ____\|\  \|\  \|\   __  \|\   ____\
#  \ \  \|\ /\ \  \|\  \ \  \___|\ \  \\\  \ \  \|\  \ \  \___|
#   \ \   __  \ \   __  \ \_____  \ \   __  \ \   _  _\ \  \
#  __\ \  \|\  \ \  \ \  \|____|\  \ \  \ \  \ \  \\  \\ \  \____
# |\__\ \_______\ \__\ \__\____\_\  \ \__\ \__\ \__\\ _\\ \_______\
# \|__|\|_______|\|__|\|__|\_________\|__|\|__|\|__|\|__|\|_______|
#                         \|_________|

# Specify XDG base directories.
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"

# When possible move some folders to XDG base directories.
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"
export MPLAYER_HOME="${XDG_CONFIG_HOME}/mplayer"
export W3M_DIR="${XDG_DATA_HOME}/w3m"
export TERMINFO="${XDG_DATA_HOME}/terminfo"
export TERMINFO_DIRS="${XDG_DATA_HOME}/terminfo:/usr/share/terminfo"

# Pick your default terminal.
if [[ -x "$( command -v st )" ]]; then
	TERMINAL="$( which st )"
	export TERMINAL
fi

# Search web with it.
if type -P firefox &> /dev/null; then
	BROWSER="$( type -P firefox )"
	export BROWSER
fi

# Can't live without vim keys.
if type -P nvim &> /dev/null; then
	EDITOR="$( which nvim )"
	test -t 0 && alias e="${EDITOR}"
	export MANPAGER="${EDITOR} +Man!"
	export EDITOR
fi

# Add path for local programs.
if [[ -d "${HOME}/.local/bin" ]]; then
	export PATH="${HOME}/.local/bin:$PATH"
fi

# Pick your devil.
if [[ -d "${XDG_CONFIG_HOME}/emacs/bin" ]]; then
	export PATH="${XDG_CONFIG_HOME}/emacs/bin:$PATH"
	export DOOMDIR="${XDG_CONFIG_HOME}/doom"
fi

# If not running interactively, don't do anything.
[[ ! -t 0 ]] && return

[[ "${DISPLAY}" ]] && shopt -s checkwinsize

# Move history's files away.
export LESSHISTFILE="${XDG_STATE_HOME}/lesshist"
export HISTFILE="${XDG_STATE_HOME}/bashhist"

shopt -s histappend
shopt -s cmdhist

export HISTCONTROL="ignoreboth"
export HISTSIZE=2000

shopt -s globstar

shopt -s autocd

# Change directory with ease.
shopt -s cdspell
shopt -s dirspell

# Don't complete on empty read line.
shopt -s no_empty_cmd_completion

shopt -s checkjobs

export FZF_DEFAULT_OPTS="--bind=alt-j:down,alt-k:up \
	--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
	--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
	--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

# Tell me what I'm missing.
PKGFILE="/usr/share/doc/pkgfile/command-not-found.bash"
[[ -r "${PKGFILE}" ]] && source "${PKGFILE}"
unset PKGFILE

# Complete bash completion.
COMPFILE="/usr/share/bash_completion/bash_completion"
[[ -r "${COMPFILE}" ]] && source "${COMPFILE}"
unset COMPFILE

# Manage colors for some cli utils.
if [[ -x "$( command -v dircolors )" ]]; then
	DIRCOLORS="${XDG_CONFIG_HOME}/dircolors"
	test -r "${DIRCOLORS}" || dircolors -p > "${DIRCOLORS}"
	eval "$( dircolors -b "${DIRCOLORS}" )"
	unset DIRCOLORS
fi

# Tell before override.
alias mv="$( which mv ) -i"
alias cp="$( which cp ) -i"

# Human readable.
alias du="$( which du ) -h"
alias df="$( which df ) -h"
alias free="$( which free ) -m"

# Make some colors.
alias grep="$( which grep ) --color=auto"
alias egrep="$( which grep ) --color=auto -E"
alias ip="$( which ip ) -color=auto"

# Always Monday.
alias cal="$( which cal ) -m"

# Is it catman? No, it's batman!
test -x "$( command -v bat )" && alias cat="$( which bat )"

# Get rid of history file.
test -x "$( command -v wget )" && alias wget="$( which wget ) --no-hsts"

# Modern ls replacement.
if [[ -x "$( command -v eza )" ]]; then
	alias l="$( which eza ) -lA --group-directories-first \
		--no-user --git --time-style \"+%y/%m/%d\""
else
	alias l="$( which ls ) -gGAh --group-directories-first --color=auto"
fi

# Operating system in terminal.
if type -P emacs &> /dev/null; then
	alias emacs="$( type -P emacsclient ) -tca \"$( type -P emacs ) -nw\""
fi

# Set colors for prompt.
NC='\[\033[00m\]'
RED='\[\033[00;31m\]'
GRN='\[\033[00;32m\]'
YLW='\[\033[00;33m\]'
BLU='\[\033[00;34m\]'
PUR='\[\033[00;35m\]'

PS1="${PUR}\u${RED}@${BLU}\h ${YLW}\w ${RED}\$ ${NC}"
export PS1

unset NC RED GRN YLW BLU PUR

# Everyone should use it.
if [[ -x "$( command -v zoxide )" ]]; then
	eval "$( zoxide init bash )"
fi
