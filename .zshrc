#!/usr/bin/env zsh
# @prog         zsh
# @lastProgVers 5.7.1
# @since        2015-12-23
# @lastChanged  2020-02-13
# @author       Mort Yao <soi@mort.ninja>

# Common settings
export EDITOR=vim
export VISUAL=vim
export GPG_TTY=$(tty)

# Personal settings
MY_FATE=rms-facts
MY_COW=duck
PLUGINS=(
    git pip emoji
    zsh-users/zsh-syntax-highlighting
)
export ZSH_HIGHLIGHT_MAXLENGTH=100
# Disable buggy url-quote-magic, etc.
# see also: https://github.com/robbyrussell/oh-my-zsh/issues/5569#issuecomment-491504337
DISABLE_MAGIC_FUNCTIONS=true

# Load prelude
source $HOME/.zsh/prelude

# Show me some fortune cookies
fate $MY_FATE | cow -W $(($COLUMNS-4)) -f $MY_COW

# Antigen: load oh-my-zsh and other plugins
source $HOME/.zsh/antigen113.zsh && checkt
antigen use oh-my-zsh
#log.p $(checkt "loaded: $fg_bold[green]oh-my-zsh$reset_color")
function {
    local i && for i in "${*[@]}"; do
        antigen bundle $i
        #log.p $(checkt "loaded bundle: $fg_bold[green]$i$reset_color")
    done
} $PLUGINS
antigen apply

# Load my theme
source $HOME/.zsh/$USER.zsh-theme
# Fix unreadable color for OTHER_WRITABLE dirs in $LS_COLORS
# see also: https://unix.stackexchange.com/q/94498/59659
eval `dircolors ~/.dircolors`

# Load extra settings
function {
    local i && for i in "${*[@]}"; do
        source $i
    done
} $HOME/.zsh/*.zsh(N) $HOME/.zsh/private/*.zsh(N)

# Initialize Projects & Source
function {
    local i && for i in "${*[@]}"; do . $i; done
} $HOME/{Projects,Source}/*.init.sh(N)

# Misc.
# Scripts & Tools
path+=("$HOME/Scripts")
# pip user
path+=("$HOME/.local/bin")
# gem executables
path=("$HOME/.gem/ruby/2.7.0/bin" "$HOME/.gem/ruby/2.6.0/bin" "$HOME/.gem/ruby/2.5.0/bin" $path)
# go
GOPATH=~/go
path+=("${GOPATH}/bin")
# cabal
CABAL_HOME="${HOME}/.cabal"
path=("${CABAL_HOME}/bin" $path)
# smackage
SMACKAGE_HOME="${HOME}/.smackage"
path+=("${SMACKAGE_HOME}/bin")
# opam configuration
# eval $(opam env)
test -r $HOME/.opam/opam-init/init.zsh && . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
# pyenv configuration
PYENV_ROOT="${HOME}/.pyenv"
path=("${PYENV_ROOT}/bin" $path)
eval "$(pyenv init --path)"
eval "$(pyenv init -)"
# ciao
# @begin(87653063)@ - Do not edit these lines - added automatically!
# You should customize CIAOPATH before this chunk if you place bundles in
# places other than ~/.ciao
if [ -x /home/soimort/Source/ciao/build/bin/ciao-env ] ; then
  eval "$(/home/soimort/Source/ciao/build/bin/ciao-env --sh)"
fi
# @end(87653063)@ - End of automatically added lines.

#log.p $(checkt 'ok')
