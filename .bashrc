export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"
export MPLAYER_HOME="${XDG_CONFIG_HOME}/mplayer"
export W3M_DIR="${XDG_DATA_HOME}/w3m"

export TERMINFO="${XDG_DATA_HOME}/terminfo"
export TERMINFO_DIRS="${XDG_DATA_HOME}/terminfo:/usr/share/terminfo"

export INPUTRC="${XDG_CONFIG_HOME}/readline/inputrc"
export LESSHISTFILE="${XDG_STATE_HOME}/lesshist"

export HISTFILE="${XDG_STATE_HOME}/bashhist"
export HISTCONTROL="ignoreboth"
export HISTSIZE=2000

export FZF_DEFAULT_OPTS="--bind=alt-j:down,alt-k:up \
	--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
	--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
	--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

if type -P st &> /dev/null; then
	TERMINAL="st"
	export TERMINAL
fi

if type -P firefox &> /dev/null; then
	BROWSER="firefox"
	export BROWSER
fi

if type -P nvim &> /dev/null; then
	EDITOR="nvim"
	export MANPAGER="nvim +Man!"
	test -t 0 && alias e="nvim"
	export EDITOR
fi

test -t 0 || return

[[ "$DISPLAY" ]] && shopt -s checkwinsize

NC='\[\033[00m\]'
RED='\[\033[00;31m\]'
YLW='\[\033[00;33m\]'
BLU='\[\033[00;34m\]'

if ! grep -q "/dev/tty" <<< "$(tty)"; then
	PS1="${BLU}\u ${YLW}\w ${RED}&> ${NC}"
	export PS1
fi

unset NC RED YLW BLU

if [[ "$(tty)" != "/dev/tty1" ]]; then
	if type -P fortune &> /dev/null && type -P cowsay &> /dev/null; then
		fortune -s | tr -d '\t' | cowsay -f bud-frogs
	fi
fi

shopt -s globstar

shopt -s autocd
shopt -s cdspell
shopt -s dirspell

shopt -s no_empty_cmd_completion

shopt -s checkjobs

shopt -s histappend
shopt -s cmdhist

if [[ -e "/usr/share/doc/pkgfile/command-not-found.bash" ]]; then
	source "/usr/share/doc/pkgfile/command-not-found.bash"
fi

if [[ -e "/usr/share/bash-completion/bash_completion" ]]; then
	source "/usr/share/bash-completion/bash_completion"
fi

if command -v dircolors &> /dev/null; then
	if [[ ! -e "${XDG_CONFIG_HOME}/dircolors" ]]; then
		dircolors -p > "${XDG_CONFIG_HOME}/dircolors"
	fi
	eval "$(dircolors -b "${XDG_CONFIG_HOME}/dircolors")"
fi

alias mv="$(type -P mv) -i"
alias cp="$(type -P cp) -i"

alias du="$(type -P du) -h"
alias df="$(type -P df) -h"
alias free="$(type -P free) -m"

alias grep="$(type -P grep) -i --color=auto"
alias egrep="$(type -P grep) -E --color=auto"
alias ip="$(type -P ip) -color=auto"

alias cal="$(type -P cal) -m"

if test type -P bat &> /dev/null; then
	if test -f "${XDG_CONFIG_HOME}/bat/config"; then
		alias cat="bat"
	else
		alias cat="bat -p"
	fi
fi

type -P wget &> /dev/null && alias wget="$(type -P wget) --no-hsts"

if type -P eza &> /dev/null; then
	alias l="eza -lA --group-directories-first --time-style \"+%y/%m/%d\" --no-user --git"
else
	alias l="ls -gGAh --group-directories-first --color=auto"
fi

if type -P zoxide &> /dev/null; then
	eval "$(zoxide init bash)"
fi
