# Mort's Cheatsheet

## Emacs

### Project workflow

#### Initialize a new project

    $ md my_project
    $ cd my_project/
    $ git init
    $ git flow init -d
    $ echo '.emacs*' > .gitignore
    $ e .gitignore

In Emacs, use `M-s r` to save the current session into `./.emacs.desktop` and `./.emacs.elscreen` before exit.

#### Start hacking on an existing project

    $ cd my_project/
    $ e

Emacs will try to restore the last session from `./.emacs.desktop` and `./.emacs.elscreen`, and save current session back into them on exiting. Automatically.

Editing certain files by initializing `emacs` with explicit parameters will not cause Emacs to restore and save sessions automatically: (unless `M-s r` is invoked)

    $ e README.md src/hello.c

### Basic commands

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

`C-<SPC>` Begin text selection

`C-g` Cancel

`C-d` Kill character

`M-d` Kill word

`M-<delete>` Kill word backwards

`C-w` Cut (kill)

`M-w` Copy

`M-y` Paste (yank)

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

`M-<` Go to top of buffer

`M->` Go to end of buffer

`M-g M-g` Go to line number

`M-x` Change mode

`M-x !` Run shell command

`M-x shell` Shell

`M-x eshell` Eshell

`M-x term`  Terminal emulation

### Customized commands

`<RET>` Newline and indent

`C-x C-c` Quit (without asking for saving buffer)

`C-S-<backspace>` Delete whole line (without putting it into kill-ring)

`M-n` Scroll view down (1 line)

`M-<down>` Scroll view down

`M-p` Scroll view up (1 line)

`M-<up>` Scroll view up

### ElScreen commands

`M-s c` `M-s C-c` Create a new screen and switch to it

`M-s C` Create a new screen with the window-configuration of the current screen

`M-s d` Create a new screen and run dired

`M-s C-f` Find file in new screen

`M-s C-r` Find file in new screen (read-only)

`M-s k` `M-s C-k` Kill current screen

`M-s M-k` Kill current screen and buffers

`M-s K` Kill other screens

`C-<tab>` `M-s n` `M-s C-n` Next screen

`C-S-<tab>` `M-s p` `M-s C-p` Previous screen

`M-s a` `M-s C-a` Toggle to the screen selected previously

`M-s '` Prompt for a screen number to switch to

`M-s "` Present a list of all screens for selection

`M-s [0-9]` Jump to the screen number 0-9

`M-s C-s` Swap current screen with previous one

`M-s w` `M-s C-w` Show a list of screens

`M-s A` Allow the user to enter a name for the current screen

`M-s m` `M-s C-m` Repeat the last message displayed in the mini-buffer

`M-s t` `M-s C-t` Display date/time

`M-s M-x` Read function name, then call it with new screen

`M-s i` Show/hide the screen number in the mode line

`M-s T` Show/hide the tab on the top of each frame

`M-s v` Display ElScreen version

`M-s b` Switch to the screen in which specified buffer is displayed

`M-s ?` Show key bindings of ElScreen and Add-On softwares

### Customized ElScreen commands

`M-s r` Remember (save current session)

`M-s <SPC>` Open all buffers in individual screens

### cua-mode

`C-x` (Active region) Cut (kill)

`C-c` (Active region) Copy

`C-v` Paste (yank)

`C-z` Undo

### emmet-mode

`C-j` Expand



## Tmux

### Split window / Create pane

`M-s h` `M-s <` Split window horizontally

`M-s v` `M-s /` Split window vertically

`M-s t` Display time

`M-s q` Display pane number

`M-s x` Kill current pane

### Select pane

`M-<up>` Select up-side pane

`M-<down>` Select down-side pane

`M-<left>` Select left-side pane

`M-<right>` Select right-side pane

`M-s ;` Select last pane

### Resize pane

`M-s <up>` Resize pane up-side

`M-s <down>` Resize pane down-side

`M-s <left>` Resize pane left-side

`M-s <right>` Resize pane right-side

### Create window

`M-s c` Create new window

`M-s &` Kill current window

`M-s k` Kill current window (no prompt)

### Select window

`C-<up>` Select previous window

`C-<down>` Select next window

`M-s [0-9]` Select the window number 0-9

### Miscellany

`M-s ?` Show key bindings of Tmux

`M-s r` Reload configuration (`.tmux.conf`)



## Git

### Git-flow commands

`git flow init -d` Initialize a new repo with the default branch structure

`git flow feature` List feature branches

`git flow feature start <name>` Start a feature branch

`git flow feature finish <name>` Finish a feature branch

`git flow feature publish <name>` Push a feature branch to the remote repo

`git flow feature pull <remote> <name>` Pull a feature branch from the remote repo

`git flow release` List release branches

`git flow release start <release>` Start a release branch

`git flow release finish <release>` Finish a release branch

`git flow hotfix` List hotfix branches

`git flow hotfix start <release>` Start a hotfix branch

`git flow hotfix finish <release>` Finish a hotfix branch

`git flow support` List support branches

`git flow support start <release> <base>` Start a support branch

### Aliases

`git co` = `git checkout`

`git br` = `git branch`

`git ci` = `git commit`

`git st` = `git status`

`git unstage` = `git reset HEAD --`

`git last` = `git log -1 HEAD`

`git ll` Show fancy Git log

`git d` = `git diff HEAD --`
