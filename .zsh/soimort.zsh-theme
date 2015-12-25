#!/usr/bin/env zsh
# Oh My Zsh theme
# @since        2015-12-23
# @lastChanged  2015-12-25
# @author       Mort Yao <soi@mort.ninja>

local ret_status="%(?:%{$fg_bold[green]%}[♥]:%{$fg_bold[red]%}[%?]%s)"
PROMPT='${ret_status}%{$fg_bold[green]%}%p %{$fg[white]%}%c $(git_prompt_info) %{$reset_color%}'
ZSH_THEME_GIT_PROMPT_PREFIX="%{$bg[white]$fg[black]%} "
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$reset_color%} %{$fg_bold[yellow]%}◆%{$reset_color%}"

TIMEFMT="$fg_bold[green][⌛]$reset_color$fg[green] ($reset_color\
$fg_bold[green]%*S$reset_color$fg[green]s : sys + $reset_color\
$fg_bold[green]%*U$reset_color$fg[green]s : user) / $reset_color\
$fg_bold[green]%*E$reset_color$fg[green]s : all = $reset_color\
$fg_bold[green]%P$reset_color$fg[green] : cpu $reset_color\
$fg_bold[green]$ %J$reset_color"
REPORTTIME=1
