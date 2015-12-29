#!/usr/bin/env zsh
# .zshrc
# @since        2015-12-23
# @lastChanged  2015-12-25
# @author       Mort Yao <soi@mort.ninja>

# Common settings
export EDITOR=emacs
export VISUAL=emacs

# Personal settings
MY_FATE=rms-facts
MY_COW=duck
PLUGINS=(
    git pip emoji
    zsh-users/zsh-syntax-highlighting
    rg3/youtube-dl
)
RECIPES=(
    becat
    resave
    translate-shell
    you-get
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
    done
} $PLUGINS
log.p $(checkt "loaded bundles: $fg_bold[green]$PLUGINS$reset_color")
function {
    local i && for i in "${*[@]}"; do
        if [ -d ~prj/$i ]; then
            antigen bundle ~prj/$i --no-local-clone
        else
            log.w "bundle not found: $i"
        fi
    done
} $RECIPES
log.p $(checkt "loaded bundles: $fg_bold[green]$RECIPES$reset_color")
antigen apply

# Load my theme
source $HOME/.zsh/$USER.zsh-theme

# Load extra settings
function {
    local i && for i in "${*[@]}"; do
        source $i
    done
} $HOME/.zsh/*.zsh(N) $HOME/.zsh/private/*.zsh(N)

# Set PATH
# pip
path+=("$HOME/.local/bin")
# linuxbrew
path+=("$HOME/.linuxbrew/bin")
# cabal
CABAL_HOME="${HOME}/.cabal"
path+=("${CABAL_HOME}/bin")
# rvm (must be put at last)
source ~/.rvm/scripts/rvm

log.p $(checkt 'ok')
