#!/usr/bin/env zsh

magic-enter() {
    if [[ -z $BUFFER ]]; then
        clear; echo $fg_bold[white]$PWD$reset_color

        if [[ $PWD =~ "/home/" ]]; then
            echo -n $fg_bold[blue]"User:\t"; git-get-author; echo -n $reset_color
        fi

        local TMP=`git-list-origin`
        if [[ -n $TMP ]]; then
            echo $fg_bold[blue]"Origin:\t$TMP$reset_color"
        fi

        TMP=`emacs-show`
        if [[ -n $TMP ]]; then
            echo $fg_bold[green]"Emacs session:\n\t$TMP$reset_color"
        fi

        git status 2>/dev/null
    fi
    zle accept-line
}
zle -N magic-enter
bindkey "^M" magic-enter
