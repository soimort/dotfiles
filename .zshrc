source ${HOME}/.zsh/antigen/antigen.zsh

# Load the oh-my-zsh's library
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh)
antigen bundle git
antigen bundle heroku
antigen bundle pip
antigen bundle lein
antigen bundle command-not-found

# Syntax highlighting bundle
antigen bundle zsh-users/zsh-syntax-highlighting

# Tell antigen that you're done
antigen apply

# Set default editor
export EDITOR=emacs
export VISUAL=emacs

# Add Linuxbrew path
export PATH="$HOME/.linuxbrew/bin:$PATH"

# Add Cabal path
export CABAL_HOME="${HOME}/.cabal"
export PATH="${CABAL_HOME}/bin:${PATH}"

# Add ~/Scripts path
export PATH="${HOME}/Scripts:${PATH}"

# Add ~/Packages path
export PATH="${HOME}/Packages/bin:${PATH}"
export LD_LIBRARY_PATH="${HOME}/Packages/lib:$LD_LIBRARY_PATH"

# Import initialization scripts
for i in $HOME/.zsh/*.sh; do source $i; done
for i in $HOME/Projects/*.sh; do source $i; done
for i in $HOME/Programs/*.sh; do source $i; done
# for i in $HOME/Source/*.sh; do source $i; done

# Import RVM (RVM PATH must be at first place)
source ~/.rvm/scripts/rvm
