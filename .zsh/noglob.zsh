#!/usr/bin/env zsh

function {
    for x in $*; do
        alias $x="noglob $x"
    done
} \
         bower rake
