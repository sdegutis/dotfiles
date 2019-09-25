sd_retval_cond () {
    local ret_val=$?
    if [[ "$ret_val" = "0" ]]; then echo -e "$1"; else echo -e "$2"; fi
    return $ret_val
}

RESET='\[\e[0m\]'     BOLD='\[\e[1m\]'
YELLOW='\[\e[33m\]'   BLUE='\[\e[34m\]'
BLACK='\[\e[30m\]'    RED='\[\e[31m\]'
PINK='\[\e[35m\]'     CYAN='\[\e[36m\]'
GREEN='\[\e[32m\]'    GRAY='\[\e[37m\]'
export PS1="$RESET\n$BOLD$BLUE(\u) \$(sd_retval_cond \"$GREEN[woo]\" \"$RED[aww \$?]\") $YELLOW(\$(date +%H:%M:%S)) $PINK\w\n$RESET$BLUE\$$RESET "

export CLICOLOR=1
export EDITOR='vim'

alias ls="ls -lah"
alias tree="tree -C"

export HISTFILESIZE=500000
export HISTSIZE=500000

# homebrew
export PATH="/usr/local/bin:$PATH"

# bash completion
source /usr/local/etc/bash_completion
