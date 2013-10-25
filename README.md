# Mort's Cheatsheet

## Emacs

### Basic commands

`C-x C-c` Quit

`C-x C-f` Find file

`C-x C-s` Save buffer

`C-x s` Save file (like save-as)

`C-x b` Switch buffer

`C-x k` Kill buffer

`C-x o` Move cursor to other window

`C-x 0` Close current window (after splitting)

`C-x 1` Close any other windows

`C-x 2` Split window horizontally

`C-x 3` Split window vertically

`C-SPC` Begin text selection

`C-g` Cancel

`C-d` Kill character

`M-d` Kill word

`M-delete` Kill word backwards

`C-w` Cut

`M-w` Copy

`M-y` Paste

`M-h` Select current paragraph

`C-s` Incremental search

`C-r` Incremental search backward

`C-M-s` Regex incremental search

`C-M-r` Regex incremental search backward

`M-%` Search/replace

`M-;` Comment/uncomment region

`C-/` `C-x u` Undo

`M-/` Complete word (like tab-complete)

`C-f` Move cursor forward

`C-b` Move cursor back

`M-f` Move cursor forward-by-word

`M-b` Move cursor backward-by-word

`C-n` Move cursor to next line

`C-p` Move cursor to prev line

`C-l` Scroll to middle and redraw screen

`C-a` Move cursor to beginning of line

`C-e` Move cursor to end of line

`M-m` Move cursor to first non-whitespace char

`M-{` Move cursor up-by-paragraph

`M-}` Move cursor down-by-paragraph

`C-v` Page down

`M-v` Page up

`M-<` Go to top of buffer

`M->` Go to end of buffer

`M-g M-g n` Go to line number n

`M-x foo-mode` Change mode

`M-x !` Run shell command

`M-x shell` Shell

`M-x eshell` Eshell

`M-x term`  Terminal emulation

### Customized commands

`C-S-backspace` Delete whole line (without putting it into kill-ring)

`M-n` Scroll view down (1 line)

`M-<down>` Scroll view down

`M-p` Scroll view up (1 line)

`M-<up>` Scroll view up

### ElScreen commands

`C-z c` `C-z C-c` Create a new screen and switch to it

`C-z C` Create a new screen with the window-configuration of the current screen

`C-z d` Create a new screen and run dired

`C-z C-f` Find file in new screen

`C-z C-r` Find file in new screen (read-only)

`C-z k` `C-z M-k` Kill current screen

`C-z M-k` Kill current screen and buffers

`C-z K` Kill other screens

`C-tab` `C-z n` `C-z C-n` Next screen

`C-S-tab` `C-z p` `C-z C-p` Previous screen

`C-z a` `C-z C-a` Toggle to the screen selected previously

`C-z '` Prompt for a screen number to switch to

`C-z "` Present a list of all screens for selection

`C-z 0..9` Jump to the screen number 0-9

`C-z C-s` Swap current screen with previous one

`C-z w` `C-z C-w` Show a list of screens

`C-z A` Allow the user to enter a name for the current screen

`C-z m` `C-z C-m` Repeat the last message displayed in the mini-buffer

`C-z t` `C-z C-t` Display date/time

`C-z M-x` Read function name, then call it with new screen

`C-z i` Show/hide the screen number in the mode line

`C-z T` Show/hide the tab on the top of each frame

`C-z v` Display ElScreen version

`C-z b` Switch to the screen in which specified buffer is displayed

`C-z ?` Show key bindings of ElScreen and Add-On softwares

### Customized ElScreen commands

`C-z e` Save current session & tabs

`C-z SPC` Open all buffers in individual screens

### Miscellany

`C-j` Expand (emmet-mode)



## Tmux

### Split window / Create pane

`M-r h` `M-r <` Split window horizontally

`M-r v` `M-r /` Split window vertically

`M-r t` Display time

`M-r q` Display pane number

`M-r x` Kill current pane

### Select pane

`M-Up` Select up-side pane

`M-Down` Select down-side pane

`M-Left` Select left-side pane

`M-Right` Select right-side pane

`M-r ;` Select last pane

### Resize pane

`M-r Up` Resize pane up-side

`M-r Down` Resize pane down-side

`M-r Left` Resize pane left-side

`M-r Right` Resize pane right-side

### Create window

`M-r c` Create new window

`M-r &` Kill current window

`M-r k` Kill current window (no prompt)

### Select window

`C-Up` Select previous window

`C-Down` Select next window

`M-r 0..9` Select window number 0-9

### Miscellany

`M-r ?` Show key bindings of Tmux

`M-r r` Reload configuration (`.tmux.conf`)
