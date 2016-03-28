#!/usr/bin/env zsh

# Zsh
alias zshconfig="$EDITOR $HOME/.zshrc"
alias .,=". $HOME/.zshrc"
alias plz='sudo $(fc -ln -1)'

# coreutils
alias cp="cp -i"
alias mv="mv -i"

# development
alias py="python3"
alias py2="python2"

# editors
alias v="vim"
alias gv="gvim"
alias suv="sudo vim"
alias e="emacs -nw"
alias em="setsid emacs"
alias sc="setsid scite"
alias susc="sudo scite"

# git
alias -- "+"="git add"
alias -- '$'="git commit -m"
alias add="git add"
alias br="git branch"
alias ci="git commit"
alias clone="git clone"
alias co="git checkout"
alias d="git diff HEAD --"
alias f="git diff"
alias ll="git ll"
alias merge="git merge"
alias pull="git pull"
alias push="git push"
alias remote="git remote"
alias rm="git rm"
alias st="git status"
alias tag="git tag"
alias k="setsid gitk"

# networking
alias i="ping -c 3 www.google.com"
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias mygeoip='geoiplookup $(myip)'
alias ssc="sslocal -c $HOME/.zsh/private/shadowsocks.json"
alias xyc="proxychains4 -q"

# Arch
alias yao='yaourt'

# GNOME
alias sysmon="setsid gnome-system-monitor"

### personal

alias rs='resave'
alias trs='trans'
alias yg='you-get'

hash -d trans=~/Projects/translate-shell
hash -d you-get=~/Projects/you-get