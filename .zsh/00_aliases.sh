# zsh
alias zshconfig="$EDITOR $HOME/.zshrc"
alias ohmyzsh="$EDITOR $ZSH/oh-my-zsh.sh"
alias .,=". $HOME/.zshrc"
alias zcompdump_clean='rm -f ~/.zcompdump*'

# directories
alias -- -='cd -'

# system
alias cp="cp -i"
alias mv="mv -i"

# editors
alias vi="vim"
alias v="vim"
alias gv="gvim"
alias em="emacs -nw"
alias e="setsid emacs"
alias sc="setsid scite"
alias sue="sudo emacs -u $USER"
alias susc="sudo scite"

# git
alias add="git add"
alias br="git br"
alias ci="git ci"
alias clone="git clone"
alias co="git co"
alias d="git diff HEAD --"
alias f="git diff"
alias ll="git ll"
alias merge="git merge"
alias pull="git pull"
alias push="git push"
alias remote="git remote"
alias rm="git rm"
alias st="git st"
alias tag="git tag"
alias k="fork gitk"

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

# desktop
alias monitor="fork gnome-system-monitor"

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

# noglob
alias rake='noglob rake'
alias bower='noglob bower'
