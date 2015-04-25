# zsh
alias zshconfig="$EDITOR $HOME/.zshrc"
alias ohmyzsh="$EDITOR $ZSH/oh-my-zsh.sh"
alias .,=". $HOME/.zshrc"
alias zcompdump_clean='rm -f ~/.zcompdump*'

# system
alias cp="cp -i"
alias mv="mv -i"
alias rm="echo Is this what you want\? Think again: rm"

# editors
alias vi="vim"
alias gv="gvim"
alias em="emacs -nw"
alias e="setsid emacs"
alias sc="setsid scite"
alias sudoe="sudo emacs -u $USER"
alias sudosc="sudo scite"

# development
alias py="python3"
alias py2="python2"
alias rb="ruby"
alias grsh="groovysh"

# networking
alias i="ping -c 3 www.google.com"
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias mygeoip='geoiplookup $(myip)' # use SINGLE quotation marks
alias import-certs="mozroots --import --ask-remove"

# productivity
alias get-shit-done="sudo get-shit-done work"
alias shit-done="sudo get-shit-done play"

# tools
alias pac="sudo pacman"
alias tweet="earthquake -c"

# misc
alias ds="du -hs * | sort -hr | head"
alias difr="diff -r" # recursively compare subdirectories
alias remove-trailing-whitespaces="sed -i 's/[ \t]*$//'"
alias remove-u-feff="perl -pi~ -CSD -e 's/^\x{feff}//'"
