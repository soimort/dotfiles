
# load X resource file and merge with current settings
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# set default cursor
xsetroot -cursor_name left_ptr

# set background image
feh --bg-scale /home/soimort/Pictures/wallpapers/default.jpg &

xscreensaver &

exec xmonad
