#!/usr/bin/env zsh

magic-enter() {
    if [[ -z $BUFFER ]]; then
        clear; echo -n $fg_bold[green]$PWD : $reset_color
        git status 2>/dev/null
    fi
    zle accept-line
}
zle -N magic-enter
bindkey "^M" magic-enter
