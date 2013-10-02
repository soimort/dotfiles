HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=()
source $ZSH/oh-my-zsh.sh

unsetopt correct_all
unsetopt share_history

EDITOR=emacs

alias zshconfig="$EDITOR $HOME/.zshrc"
alias ohmyzsh="$EDITOR $ZSH/oh-my-zsh.sh"
alias .,=". $HOME/.zshrc"

alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

alias vi="vim"
alias gv="gvim"
alias em="emacs -nw"
alias e="setsid emacs"
alias sc="scite"

alias py="python3"
alias py2="python2"
alias rb="ruby"
alias grsh="groovysh"

alias i="ping -c 3 www.google.com"
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias mygeoip='geoiplookup $(myip)' # Do NOT change single quotation marks here!

# PATH for Languages
J_PATH="${HOME}/Programs/j64-701"
MOSML_PATH="/opt/mosml"

# PATH for Package managers
CABAL_HOME="${HOME}/.cabal"
GEM_HOME="${HOME}/.gem/ruby/1.9.1"

# PATH for PaaS SDKs
GAE_PATH="${HOME}/Programs/google_appengine"
HEROKU_PATH="/usr/local/heroku"

# Set PATH for local programs
export PATH="${CABAL_HOME}/bin:${PATH}"
export PATH="${PATH}:${HOME}/Programs/bin:${J_PATH}/bin:${MOSML_PATH}/bin:${GEM_HOME}/bin:${GAE_PATH}/bin:${HEROKU_PATH}/bin"

# Import RVM (must be put after PATH setting)
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# Bootstrap ~/.zsh
for i in $HOME/.zsh/*.sh; do
    source $i
done

# Bootstrap ~/src
for i in $HOME/src/*.sh; do
    source $i
done
