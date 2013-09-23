HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=()
source $ZSH/oh-my-zsh.sh

unsetopt correct_all
unsetopt share_history

EDITOR=vim

fork() { (setsid "$@" &); }

alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"

alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

alias i="ping -c 3 www.google.com"
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias mygeoip='geoiplookup $(myip)'

alias vi="vim"
alias gv="gvim"
alias em="emacs -nw"
alias e="setsid emacs"
alias sc="scite"

alias py="python3"
alias py2="python2"

# PATH for Languages
J_PATH="${HOME}/Programs/j64-701"
MOSML_PATH="/opt/mosml"

# PATH for Package managers
CABAL_HOME="${HOME}/.cabal"
GEM_HOME="${HOME}/.gem/ruby/1.9.1"

# PATH for PaaS SDKs
GAE_PATH="${HOME}/Programs/google_appengine"
HEROKU_PATH="/usr/local/heroku"

# Set PATH
export PATH="${CABAL_HOME}/bin:${PATH}"
export PATH="${PATH}:${HOME}/Programs/bin:${J_PATH}/bin:${MOSML_PATH}/bin:${GEM_HOME}/bin:${GAE_PATH}/bin:${HEROKU_PATH}/bin"

# Import RVM after PATH is set
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# Load extension functions
source $HOME/.zshrc_extensions

# Load private environment variables
source $HOME/.zshrc_private

alias jpcsp="(cd ${HOME}/Programs/jpcsp-linux-amd64;./start-linux-amd64.sh)"

alias becat="${HOME}/Projects/becat/becat"

alias br2html="${HOME}/Projects/breakdown/br2html.awk"
