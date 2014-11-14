HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
plugins=()
source $ZSH/oh-my-zsh.sh

unsetopt correct_all
unsetopt share_history

alias zshconfig="$EDITOR $HOME/.zshrc"
alias ohmyzsh="$EDITOR $ZSH/oh-my-zsh.sh"
alias .,=". $HOME/.zshrc"

alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

EDITOR=emacs
export VISUAL=emacs
alias vi="vim"
alias gv="gvim"
alias em="emacs -nw"
alias e="setsid emacs"
alias sc="setsid scite"
alias sudoe="sudo emacs -u $USER"
alias sudosc="sudo scite"

alias py="python3"
alias py2="python2"
alias rb="ruby"
alias grsh="groovysh"

alias i="ping -c 3 www.google.com"
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias mygeoip='geoiplookup $(myip)' # Do NOT change single quotation marks here!

alias difr="diff -r"

alias trs="trans"

alias tweet="earthquake -c"

alias get-shit-done="sudo get-shit-done work"
alias shit-done="sudo get-shit-done play"

alias import-certs="mozroots --import --ask-remove"

alias remove-trailing-whitespaces="sed -i 's/[ \t]*$//'"

alias clean-zcompdump='rm -f ~/.zcompdump*'

alias pac="sudo pacman"

alias scheme="rlwrap scheme"
alias guile="rlwrap guile"
alias sml="rlwrap sml"
alias mosml="rlwrap mosml"
alias maxima="rlwrap maxima"
alias sbcl="rlwrap sbcl"
alias ecl="rlwrap ecl"
alias lisp="rlwrap lisp"
alias cmucl="rlwrap cmucl"
alias ccl="rlwrap ccl"
alias ccl64="rlwrap ccl64"

# Cabal (Haskell package manager)
CABAL_HOME="${HOME}/.cabal"
export PATH="${CABAL_HOME}/bin:${PATH}"

# Gem (Ruby package manager)
GEM_HOME="${HOME}/.gem/ruby/1.9.1"
export PATH="${PATH}:${GEM_HOME}/bin"

# Linuxbrew package manager
export PATH="$HOME/.linuxbrew/bin:$PATH"
export LD_LIBRARY_PATH="$HOME/.linuxbrew/lib:$LD_LIBRARY_PATH"

# ATS2 programming language
export PATSHOME="/usr/lib/ats2-postiats-0.0.5"

# Hope programming language
export HOPEPATH="/usr/share/hope/lib"

# Heroku
export PATH="${PATH}:/usr/local/heroku/bin"

# My ~/Scripts
export PATH="${HOME}/Scripts:${PATH}"

# Bootstrap ~/.zsh
for i in $HOME/.zsh/*.sh; do
    source $i
done

# Bootstrap ~/Programs
for i in $HOME/Programs/*.sh; do
    source $i
done

# Bootstrap ~/Sources
for i in $HOME/Source/*.sh; do
    source $i
done

# Bootstrap ~/Projects
for i in $HOME/Projects/*.sh; do
    source $i
done

# Hitman
for i in $HOME/Projects/hitman/*/_build/bin; do
    export PATH="$i:$PATH"
done

# Import RVM (must be put after PATH setting)
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
