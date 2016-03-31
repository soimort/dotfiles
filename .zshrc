#!/usr/bin/env zsh
# .zshrc
# @since        2015-12-23
# @lastChanged  2015-03-31
# @author       Mort Yao <soi@mort.ninja>

# Common settings
export EDITOR=vim
export VISUAL=vim

# Personal settings
MY_FATE=rms-facts
MY_COW=duck
PLUGINS=(
    git pip emoji
    zsh-users/zsh-syntax-highlighting
    rg3/youtube-dl
)

# Load prelude
source $HOME/.zsh/prelude

# Show me some fortune cookies
fate $MY_FATE | cow -W $(($COLUMNS-4)) -f $MY_COW

# Antigen: load oh-my-zsh and other plugins
source $HOME/.zsh/antigen/antigen.zsh && checkt
antigen use oh-my-zsh
log.p $(checkt "loaded: $fg_bold[green]oh-my-zsh$reset_color")
function {
    local i && for i in "${*[@]}"; do
        antigen bundle $i
        log.p $(checkt "loaded bundle: $fg_bold[green]$i$reset_color")
    done
} $PLUGINS
antigen apply

# Load my theme
source $HOME/.zsh/$USER.zsh-theme

# Load extra settings
function {
    local i && for i in "${*[@]}"; do
        source $i
    done
} $HOME/.zsh/*.zsh(N) $HOME/.zsh/private/*.zsh(N)

# Initialize Projects & Source
function {
    local i && for i in "${*[@]}"; do . $i; done
} $HOME/{Projects,Source}/*.init.sh

# Misc.
# Scripts & Tools
path+=("$HOME/Scripts" "$HOME/Tools")
# pip user
path+=("$HOME/.local/bin")
# cabal
CABAL_HOME="${HOME}/.cabal"
path+=("${CABAL_HOME}/bin")
# rvm (must be put at last)
source ~/.rvm/scripts/rvm

log.p $(checkt 'ok')
