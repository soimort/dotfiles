
# load X resource file and merge with current settings
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# set default cursor
xsetroot -cursor_name left_ptr

# set background image
feh --bg-scale /home/soimort/Pictures/wallpapers/default.jpg &

# start screensaver in background
xscreensaver &

# start SSH agent
eval $(ssh-agent)

# start ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon --daemonize --replace --xim

exec xmonad
