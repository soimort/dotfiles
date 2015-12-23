#!/usr/bin/env zsh
# Oh My Zsh theme
# @since        2015-12-23
# @lastChanged  2015-12-23
# @author       Mort Yao <soi@mort.ninja>

PROMPT='%(?:%{$fg_bold[green]%}(♥・λ・:%{$fg_bold[red]%}(♥[%?]・ω・%s)%{$fg_bold[green]%}%p %{$fg[white]%}%c %{$fg_bold[blue]%}$(git_prompt_info)%{$fg_bold[blue]%} % %{$reset_color%}'
ZSH_THEME_GIT_PROMPT_PREFIX="[%{$fg[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%}]"
