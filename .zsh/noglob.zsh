#!/usr/bin/env zsh
local noglob_cmd_list
noglob_cmd_list=(
    bower rake
)

function {
    local i && for i in "${*[@]}"; do
        alias "$i"="noglob $i"
    done
} $noglob_cmd_list
