#!/bin/sh
dmesg -w > `date +"%Y%m%d-%H%M"`.dmesg &
tmux new-session -d 'htop'
tmux split-window -v 'iotop'
tmux -2 attach-session -d
