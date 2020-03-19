autoload -Uz compinit && compinit

# nicer than bash
PROMPT=$'\n'                                         # \n
PROMPT+='%{%B%F{blue}%}(%n)%{%f%b%} '                # (username)
PROMPT+='%(?.'                                       # if last exit was 0
PROMPT+='%{%B%F{green}%}[woo]%{%f%b%}'               #   [woo]
PROMPT+='.'                                          # else
PROMPT+='%{%B%F{red}%}[aww %?]%{%f%b%}'              #   [aww 127]
PROMPT+=') '                                         # endif
PROMPT+='%{%B%F{yellow}%}(%D{%L:%M:%S %p})%{%f%b%} ' # (date)
PROMPT+='%{%B%F{magenta}%}%~%{%f%b%} '               # (cwd)
PROMPT+=$'\n''%{%B%F{blue}%}$%{%f%b%} '              # \n $

# live clock in prompt
TMOUT=1
TRAPALRM() {
  zle reset-prompt
}

# make ctrl-e/a (etc) work again
bindkey -e

# make alt-backspace (etc) work
autoload -U select-word-style
select-word-style bash

export CLICOLOR=1
export EDITOR='vim'

alias LS="ls -lah"
alias tree="tree -C"

export HISTSIZE=500000

# homebrew
export PATH="/usr/local/bin:$PATH"

# zsh completion
export fpath=(/usr/local/share/zsh-completions $fpath)

# ruby for github pages
PATH="$HOME/.gem/ruby/2.6.0/bin:$PATH"
