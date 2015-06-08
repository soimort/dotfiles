# zsh history setting
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

# import oh-my-zsh
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=()
DISABLE_AUTO_UPDATE=true
source $ZSH/oh-my-zsh.sh
unsetopt correct_all
unsetopt share_history

LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"

# import rvm
source ~/.rvm/scripts/rvm

# default editor
EDITOR=emacs
export VISUAL=emacs

# linuxbrew setting
export PATH="$HOME/.linuxbrew/bin:$PATH"
#export LD_LIBRARY_PATH="$HOME/.linuxbrew/lib:$LD_LIBRARY_PATH"

# cabal setting
CABAL_HOME="${HOME}/.cabal"
export PATH="${CABAL_HOME}/bin:${PATH}"

# (non-rvm) gem setting
GEM_HOME="${HOME}/.gem/ruby/1.9.1"
export PATH="${PATH}:${GEM_HOME}/bin"

# ~/Packages setting
export PATH="${HOME}/Packages/bin:${PATH}"
export LD_LIBRARY_PATH="${HOME}/Packages/lib:$LD_LIBRARY_PATH"

# ~/Scripts setting
export PATH="${HOME}/Scripts:${PATH}"

# import ~/Programs
for i in $HOME/Programs/*.sh; do
    source $i
done

# import ~/Projects
for i in $HOME/Projects/*.sh; do
    source $i
done

# import ~/Sources
for i in $HOME/Source/*.sh; do
    source $i
done

# import ~/.zsh
for i in $HOME/.zsh/*.sh; do
    source $i
done
