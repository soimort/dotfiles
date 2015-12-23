#!/usr/bin/env zsh
# .zshrc
# @since        2015-12-23
# @lastChanged  2015-12-23
# @author       Mort Yao <soi@mort.ninja>

source ${HOME}/.zsh/antigen/antigen.zsh

antigen use oh-my-zsh
antigen bundle git
antigen bundle heroku
antigen bundle pip
antigen bundle lein
antigen bundle emoji

antigen bundle zsh-users/zsh-syntax-highlighting

antigen apply

for x in ${HOME}/.zsh/*.zsh; do source $x; done

# Load my theme
source ${HOME}/.zsh/soimort.zsh-theme

# Common environment variables
export EDITOR=emacs
export VISUAL=emacs

# Append PATH: pip
path+=("$HOME/.local/bin")

# Append PATH: linuxbrew
path+=("$HOME/.linuxbrew/bin")

# Append PATH: cabal
CABAL_HOME="${HOME}/.cabal"
path+=("${CABAL_HOME}/bin")

# Import rvm (must be put at last)
source ~/.rvm/scripts/rvm
