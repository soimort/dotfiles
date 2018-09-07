#!/usr/bin/env zsh

# unbind ^R and ^S
bindkey -r "^R"
bindkey -r "^S"

# disable url-quote-magic
zstyle ':urlglobber' url-other-schema
