#!/bin/sh
tmux new-session -d 'htop'
tmux split-window -v 'iotop'
tmux -2 attach-session -d 
