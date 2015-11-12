#!/bin/sh
tmux new-session -d 'iotop'
tmux split-window -v 'watch sensors'
tmux split-window -h
tmux -2 attach-session -d 
