# Mort's Cheatsheet

**[WARNING] This configuration set is a very personal thing and comes with definitely NO warranty. It may eat your hamster!**

## Package initialization

### (via git submodule, could be outdated)

    $ git submodule init && git submodule update

### Vim/Vundle (no submodule, always updated)

(1)

    $ git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

(2) Launch vim and run `:PluginInstall`.

### Tmux/TPM (no submodule, always updated)

(1)

    $ git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

(2) Reload tmux environment so TPM is sourced:

    $ tmux source ~/.tmux.conf

(3) In tmux, press `prefix + I` to fetch plugins.

### Emacs/ELPA

<kbd>M-x package-install RET elscreen RET</kbd> Install [ElScreen](https://melpa.org/#/elscreen)



## Emacs

### Project workflow

#### Initialize a new project

    $ md my_project
    $ cd my_project/
    $ git init
    $ git flow init -d
    $ echo '.emacs*' > .gitignore
    $ e .gitignore

In Emacs, use <kbd>M-s r</kbd> to save the current session into `./.emacs.desktop` and `./.emacs.elscreen` before exit.

#### Start hacking on an existing project

    $ cd my_project/
    $ e

Emacs will try to restore the last session from `./.emacs.desktop` and `./.emacs.elscreen`, and save current session back into them on exiting. Automatically.

Editing certain files by initializing `emacs` with explicit parameters will not cause Emacs to restore and save sessions automatically: (unless <kbd>M-s r</kbd> is invoked)

    $ e README.md src/hello.c

### Basic commands

(See also: <http://www.emacswiki.org/emacs/EmacsNewbieKeyReference>)

<kbd>C-x C-f</kbd> Find file

<kbd>C-x C-v RET</kbd> Reload file

<kbd>C-x C-s</kbd> Save buffer

<kbd>C-x s</kbd> Save file (like save-as)

<kbd>C-x b</kbd> Switch buffer

<kbd>C-x k</kbd> Kill buffer

<kbd>C-x h</kbd> Select entire buffer

<kbd>C-x o</kbd> Move cursor to other window

<kbd>C-x 0</kbd> Close current window (after splitting)

<kbd>C-x 1</kbd> Close any other windows

<kbd>C-x 2</kbd> Split window vertically

<kbd>C-x 3</kbd> Split window horizontally

<kbd>C-g</kbd> Cancel

<kbd>C-d</kbd> Kill character

<kbd>M-d</kbd> Kill word

<kbd>M-\<delete\></kbd> Kill word backwards

<kbd>C-w</kbd> Cut (kill)

<kbd>M-w</kbd> Copy

<kbd>M-y</kbd> Paste (yank)

<kbd>M-h</kbd> Select current paragraph

<kbd>C-s</kbd> Incremental search

<kbd>C-r</kbd> Incremental search backward

<kbd>C-M-s</kbd> Regex incremental search

<kbd>C-M-r</kbd> Regex incremental search backward

<kbd>M-%</kbd> Search/replace

<kbd>M-;</kbd> Comment/uncomment region

<kbd>C-/</kbd> <kbd>C-x u</kbd> Undo

<kbd>M-/</kbd> Complete word (like tab-complete)

<kbd>C-f</kbd> Move cursor forward

<kbd>C-b</kbd> Move cursor back

<kbd>M-f</kbd> Move cursor forward-by-word

<kbd>M-b</kbd> Move cursor backward-by-word

<kbd>C-n</kbd> Move cursor to next line

<kbd>C-p</kbd> Move cursor to prev line

<kbd>C-l</kbd> Scroll to middle and redraw screen

<kbd>C-a</kbd> Move cursor to beginning of line

<kbd>C-e</kbd> Move cursor to end of line

<kbd>M-m</kbd> Move cursor to first non-whitespace char

<kbd>M-{</kbd> Move cursor up-by-paragraph

<kbd>M-}</kbd> Move cursor down-by-paragraph

<kbd>M-\<</kbd> Go to top of buffer

<kbd>M-\></kbd> Go to end of buffer

<kbd>M-g M-g</kbd> Go to line number

<kbd>M-$</kbd> Check and correct spelling of the word at point

<kbd>M-!</kbd> Run shell command

<kbd>M-|</kbd> Run shell command with region contents as input

<kbd>M-&</kbd> Run shell command asynchronously

<kbd>M-x [mode-name]</kbd> Change mode

<kbd>M-x cd</kbd> Change directory

<kbd>M-x shell</kbd> Shell

<kbd>M-x eshell</kbd> Eshell

<kbd>M-x term</kbd> Terminal emulation

<kbd>C-\\</kbd> Enable/disable input method (e.g. <kbd>C-\\ greek</kbd>)

<kbd>C-\<SPC\></kbd> Enable/disable IBus input method

<kbd>F11</kbd> Fullscreen

### Nonstandard commands

<kbd>M-n</kbd> Scroll view down (by 1 line)

<kbd>M-\<down\></kbd> Scroll view down

<kbd>M-p</kbd> Scroll view up (by 1 line)

<kbd>M-\<up\></kbd> Scroll view up

<kbd>\<RET\></kbd> Newline and indent

<kbd>C-S-\<backspace\></kbd> Delete whole line (without putting it into kill-ring)

<kbd>C-c c</kbd> Copy entire buffer to clipboard

<kbd>C-x C-c</kbd> Quit (without asking for saving buffer)

### ElScreen commands

<kbd>M-s c</kbd> <kbd>M-s C-c</kbd> Create a new screen and switch to it

<kbd>M-s C</kbd> Create a new screen with the window-configuration of the current screen

<kbd>M-s d</kbd> Create a new screen and run dired

<kbd>M-s f</kbd> <kbd>M-s C-f</kbd> Find file in new screen

<kbd>M-s C-r</kbd> Find file in new screen (read-only)

<kbd>M-s k</kbd> <kbd>M-s C-k</kbd> Kill current screen

<kbd>M-s M-k</kbd> Kill current screen and buffers

<kbd>M-s K</kbd> Kill other screens

<kbd>C-\<tab\></kbd> <kbd>M-s n</kbd> <kbd>M-s C-n</kbd> Next screen

<kbd>C-S-\<tab\></kbd> <kbd>M-s p</kbd> <kbd>M-s C-p</kbd> Previous screen

<kbd>M-s a</kbd> <kbd>M-s C-a</kbd> Toggle to the screen selected previously

<kbd>M-s '</kbd> Prompt for a screen number to switch to

<kbd>M-s "</kbd> Present a list of all screens for selection

<kbd>M-s [0-9]</kbd> Jump to the screen number 0-9

<kbd>M-s \<backspace\></kbd> <kbd>M-s C-s</kbd> Swap current screen with previous one

<kbd>M-s w</kbd> <kbd>M-s C-w</kbd> Show a list of screens

<kbd>M-s A</kbd> Allow the user to enter a name for the current screen

<kbd>M-s m</kbd> <kbd>M-s C-m</kbd> Repeat the last message displayed in the mini-buffer

<kbd>M-s t</kbd> <kbd>M-s C-t</kbd> Display date/time

<kbd>M-s M-x</kbd> Read function name, then call it with new screen

<kbd>M-s i</kbd> Show/hide the screen number in the mode line

<kbd>M-s T</kbd> Show/hide the tab on the top of each frame

<kbd>M-s v</kbd> Display ElScreen version

<kbd>M-s b</kbd> Switch to the screen in which specified buffer is displayed

<kbd>M-s ?</kbd> Show key bindings of ElScreen and Add-On softwares

### Nonstandard ElScreen commands

<kbd>M-s r</kbd> Remember (save current session)

<kbd>M-s \<SPC\></kbd> Open all buffers in individual screens

<kbd>M-s h</kbd> Toggle window split

### CUA mode

(Active region) <kbd>C-x</kbd> Cut (kill)

(Active region) <kbd>C-c</kbd> Copy

<kbd>C-v</kbd> Paste (yank)

<kbd>C-z</kbd> Undo

<kbd>C-g C-z</kbd> Redo

### Python mode

<kbd>C-S-c ></kbd> Indent 4 spaces to the right

<kbd>C-S-c <</kbd> Indent 4 spaces to the left

### Emmet mode

<kbd>C-j</kbd> Expand

### Flyspell mode

<kbd>C-c $</kbd> Correct word at point or save word to dictionary

### Customized commands

<kbd>C-` f</kbd> Find file in new screen (= <kbd>M-s C-f</kbd>)

<kbd>C-` d</kbd> Enable Flyspell mode (= <kbd>M-x flyspell-mode</kbd>)

<kbd>C-` s</kbd> Enable Flyspell mode for comments and strings only (= <kbd>M-x flyspell-prog-mode</kbd>)

<kbd>C-` w</kbd> Close current window (= <kbd>C-x 0</kbd>)

<kbd>C-` v</kbd> Split window vertically (= <kbd>C-x 2</kbd>)

<kbd>C-` h</kbd> Split window horizontally (= <kbd>C-x 3</kbd>)

<kbd>C-` TAB</kbd> Use <code>clang-format</code> to format selected region or current line



## Tmux

### Create / kill pane

<kbd>M-s 3</kbd> <kbd>M-s |</kbd> Split window horizontally

<kbd>M-s 2</kbd> <kbd>M-s -</kbd> Split window vertically

<kbd>M-s x</kbd> Kill current pane

<kbd>M-s 0</kbd> Kill current pane (no prompt!)

### Select pane

<kbd>M-s \<left\></kbd> Select left-side pane

<kbd>M-s \<right\></kbd> Select right-side pane

<kbd>M-s \<down\></kbd> Select down-side pane

<kbd>M-s \<up\></kbd> Select up-side pane

<kbd>M-s ;</kbd> Select last pane

### Resize pane

<kbd>M-h</kbd> Resize pane left-side

<kbd>M-l</kbd> Resize pane right-side

<kbd>M-j</kbd> Resize pane down-side

<kbd>M-k</kbd> Resize pane up-side

### Create / kill window

<kbd>M-s c</kbd> Create new window

<kbd>M-s &</kbd> Kill current window

<kbd>M-s k</kbd> Kill current window (no prompt!)

### Select window

<kbd>C-\<up\></kbd> Select previous window

<kbd>C-\<down\></kbd> Select next window

### Miscellany

<kbd>M-s r</kbd> Reload configuration (`.tmux.conf`)

<kbd>M-s t</kbd> Display time

<kbd>M-s q</kbd> Display pane number

<kbd>M-s ?</kbd> Show key bindings of Tmux



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

### Tags / Releases

`git tag -a vX.Y.Z -m 'version X.Y.Z' [commit]` Create a new tag

`git push origin --tags` Push tags

### Aliases

`git co` = `git checkout`

`git br` = `git branch`

`git ci` = `git commit`

`git st` = `git status`

`git unstage` = `git reset HEAD --`

`git last` = `git log -1 HEAD`

`git ll` Show fancy Git log

`git d` = `git diff HEAD --`



## Xmonad

### Customized commands

<kbd>Win-\<down\></kbd> Next workspace

<kbd>Win-\<up\></kbd> Previous workspace

<kbd>Win-\<right\></kbd> Move window to next workspace

<kbd>Win-\<left\></kbd> Move window to previous workspace

<kbd>Win-Shift-\<down\></kbd> Next screen

<kbd>Win-Shift-\<up\></kbd> Previous screen

<kbd>Win-Shift-\<right\></kbd> Move window to next screen

<kbd>Win-Shift-\<left\></kbd> Move window to previous screen

<kbd>Win-\<tab\></kbd> Toggle workspaces

<kbd>Win-Shift-L</kbd> Lock screen

<kbd>Win-Shift-P</kbd> Take a screenshot
