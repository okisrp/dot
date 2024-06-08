export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_CONFIG_HOME="${HOME}/.config"

export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"

export MPLAYER_HOME="${XDG_CONFIG_HOME}/mplayer"

if [[ -d "${HOME}/.local/bin" ]]; then
  export PATH="${HOME}/.local/bin:$PATH"
fi

if [[ -x "$(command -v nvim)" ]]; then
  export EDITOR="$(which nvim)"
  export MANPAGER=""$(which nvim)" +Man!"
fi

if [[ -d "${HOME}/.config/emacs/bin" ]]; then
  export PATH="${HOME}/.config/emacs/bin:$PATH"
  export DOOMDIR="${HOME}/.config/doom"
fi

export NVM_DIR="${XDG_CONFIG_HOME}/nvm"
[[ -s "${NVM_DIR}/nvm.sh" ]] && source "${NVM_DIR}/nvm.sh"
[[ -s "${NVM_DIR}/bash_completion" ]] && source "${NVM_DIR}/bash_completion"

[[ ! -t 0 ]] && return

[[ "${DISPLAY}" ]] && shopt -s checkwinsize

LESSHISTFILE="${XDG_STATE_HOME}/less/history"

shopt -s histappend

HISTFILE="${XDG_STATE_HOME}/bash/history"
HISTSIZE=2000

shopt -s autocd
shopt -s cdspell

bind "set completion-ignore-case on"

bind '"\ek": previous-history'
bind '"\ej": next-history'
bind '"\ea": kill-whole-line'

export FZF_DEFAULT_OPTS="--bind=alt-j:down,alt-k:up \
  --color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
  --color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
  --color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

if [[ -r "/usr/share/doc/pkgfile/command-not-found.bash" ]]; then
  source "/usr/share/doc/pkgfile/command-not-found.bash"
fi

if [[ -r "/usr/share/bash_completion/bash_completion" ]]; then
  source "/usr/share/bash_completion/bash_completion"
fi

if [[ -x "$(command -v dircolors)" ]]; then
  DIRCOLORS="${XDG_CONFIG_HOME}/dircolors"
  test -r "${DIRCOLORS}" || dircolors -p > "${DIRCOLORS}"
  eval "$(dircolors -b "${DIRCOLORS}")"
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

test -x "$(command -v bat)" && alias cat="$(which bat)"

test -x "$(command -v wget)" && alias wget="$(which wget) --no-hsts"

if [[ -x "$(command -v eza)" ]]; then
  alias l="$(which eza) -lA --group-directories-first \
    --no-user --git --time-style \"+%y/%m/%d\""
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

unset RST RED GRN YLW BLU PUR PGB DIRCOLORS

if [[ -x "$(command -v zoxide)" ]];then
  eval "$(zoxide init bash)"
fi
