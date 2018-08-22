# Just my bloody forks


## `mi-caja` (the file manager forked from `caja`)

Depends on the bloody: `mate-desktop`

Feature tweaks:

1. Load the bloody last-session on startup.
2. Hide the bloody close button on tabs.


## `moe` (the image viewer forked from `eom`)

Depends on the bloody: `mate-desktop`

Feature tweaks:

1. Get rid of the bloody menubar.
2. Do not confirm unsaved images when bloody closing.


## `vlc` (the media player)

Feature tweaks:

1. Do not show the bloody "Clean Playlist" button in the context menu.
2. Disable the bloody one-click list sorting.
3. Do not bloody repeat a non-existing item in "Loop one" mode (which is just hogging CPU and completely freezes the `vlc` process).
   * Related issues: <https://forum.videolan.org/viewtopic.php?t=140820>


## `gimp-nostalgia` (the image editor)

Feature tweaks:

1. Keep the bloody toolbox in the traditional 2.8 fashion (which I was used to).
2. Enable the bloody eraser on a layer with locked alpha channel (e.g., floating selection).
   * Related issues: [#795877](https://bugzilla.gnome.org/show_bug.cgi?id=795877), [gitlab#1923](https://gitlab.gnome.org/GNOME/gimp/issues/1923)
   * Regression commits: [d81ea13](https://github.com/GNOME/gimp/commit/d81ea1315eb6d5915ab93ee8d5fa25f228e5c90e) (GIMP 2.9+), [1910ff8](https://github.com/GNOME/gimp/commit/1910ff8b1ab6f19b57a8f2ab86b2c977576995f3) (GIMP 2.10.2+)
3. Let the bloody Gradient tool always be instant (as in GIMP 2.8, no more bloody `Shift` needed).
   * Related documentation: <https://docs.gimp.org/2.9/en/gimp-tool-blend.html>
4. Let the bloody Free Selection tool immediately commit (as in GIMP 2.8, no bloody extra `Enter` or double-click needed).
   * Related issues: [#784772](https://bugzilla.gnome.org/show_bug.cgi?id=784772), [#785781](https://bugzilla.gnome.org/show_bug.cgi?id=785781), [#787253](https://bugzilla.gnome.org/show_bug.cgi?id=787253)
5. Recover the bloody zoom focusing behavior (reverting [ef2cf21](https://github.com/GNOME/gimp/commit/ef2cf21f109007e722138feb2f945688c535085f) which implements the suggestion in Bug [#796252](https://gitlab.gnome.org/GNOME/gimp/issues/1477)).


## `scite-gtk2` (the text editor)

Feature tweaks:

1. Build with the bloody gtk2.
2. Use "`?`" instead of the bloody long "`Untitled`" string.


## `gnome-settings-daemon`

Depends on the bloody: `gnome-desktop`

Feature tweaks:

* Stop the bloody annoying "Battery Low" warning!
