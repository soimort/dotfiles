#!/bin/sh

#
# ~/.xinitrc
#
# Called by startx and xinit
#

ibus-daemon -x -d
tint2 &
devilspie2 &
exec ck-launch-session dbus-launch --sh-syntax --exit-with-session openbox-session
