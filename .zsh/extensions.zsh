#!/usr/bin/env zsh

magic-enter() {
    if [[ -z $BUFFER ]]; then
        git status
    fi
    zle accept-line
}
zle -N magic-enter
bindkey "^M" magic-enter
