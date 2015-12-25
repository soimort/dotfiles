#!/usr/bin/env zsh
local -a noglob_cmd_list=(
    bower rake
)

function {
    local i
    for i in "${*[@]}"; do
        alias "$i"="noglob $i"
    done
} $noglob_cmd_list
