# My Programs



## mate-terminal

Enable switching tabs with Ctrl-(Shift-)Tab:

```
$ gsettings set org.mate.terminal.global ctrl-tab-switch-tabs true
```



## mi-caja (file manager forked from caja)

Enable switching tabs with Ctrl-(Shift-)Tab:

```
$ gsettings set org.mate.caja.preferences ctrl-tab-switch-tabs true
```

Dependencies: `mate-desktop`, `mate-common`

Sources:

- **1.16** (***GTK+2***) https://github.com/soimort/caja
- **1.23.2** (***GTK+3***) https://github.com/soimort/mi-caja

Feature tweaks:

1. Load last-session on startup (for 1.16 only since it does not have the `-t` option).
2. Log all tab activities.
3. Hide the close button on tabs.



## `moe` (image viewer forked from eom)

Get rid of toolbar and statusbar completely:

```
$ gsettings set org.mate.eom.ui toolbar false
$ gsettings set org.mate.eom.ui statusbar false
```

Enable the `fullscreen` plugin (activating fullscreen mode with double-click):

```
$ gsettings set org.mate.eom.plugins active-plugins "['fullscreen']"
```

Dependencies: `mate-desktop`, `mate-common`

Sources:

- **1.16** (***GTK+2***) https://github.com/soimort/moe.git#branch=1.16
-

Feature tweaks:

1. Get rid of the menubar.
2. Do not confirm unsaved images when closing.



## vlc (media player)

Version: **3.0.3-4**

Feature tweaks:

1. Do not show the "Clean Playlist" button in the context menu.
2. Disable the one-click list sorting.
3. Do not repeat a non-existing item in "Loop one" mode (which is just hogging CPU and completely freezes the `vlc` process).
   * Related issues: <https://forum.videolan.org/viewtopic.php?t=140820>



## scite-gtk2 (text editor)

Version: **4.0.5-1**

Feature tweaks:

1. Build with ***GTK+2***.
2. Use "`?`" instead of the verbose "`Untitled`" string.



## <del>gimp-nostalgia (image editor)</del> JUST USE GIMP 2.8.22!

Version: **2.10.4-2**

Feature tweaks:

1. Keep the toolbox in the traditional 2.8 fashion (which I was used to).
2. Enable the eraser on a layer with locked alpha channel (e.g., floating selection).
   * Related issues: [#795877](https://bugzilla.gnome.org/show_bug.cgi?id=795877), [gitlab#1923](https://gitlab.gnome.org/GNOME/gimp/issues/1923)
   * Regression commits: [d81ea13](https://github.com/GNOME/gimp/commit/d81ea1315eb6d5915ab93ee8d5fa25f228e5c90e) (GIMP 2.9+), [1910ff8](https://github.com/GNOME/gimp/commit/1910ff8b1ab6f19b57a8f2ab86b2c977576995f3) (GIMP 2.10.2+)
3. Let the Gradient tool always be instant (as in GIMP 2.8, no more `Shift` needed).
   * Related documentation: <https://docs.gimp.org/2.9/en/gimp-tool-blend.html>
4. Let the Free Selection tool immediately commit (as in GIMP 2.8, no extra `Enter` or double-click needed).
   * Related issues: [#784772](https://bugzilla.gnome.org/show_bug.cgi?id=784772), [#785781](https://bugzilla.gnome.org/show_bug.cgi?id=785781), [#787253](https://bugzilla.gnome.org/show_bug.cgi?id=787253)
5. Recover the zoom focusing behavior (reverting [ef2cf21](https://github.com/GNOME/gimp/commit/ef2cf21f109007e722138feb2f945688c535085f) which implements the suggestion in Bug [#796252](https://gitlab.gnome.org/GNOME/gimp/issues/1477)).



## gnome-settings-daemon

Dependencies: `gnome-desktop`

Version: **3.30.1.2-1**

Feature tweaks:

* Remove the "Battery Low" warning.
