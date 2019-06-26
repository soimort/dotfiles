import XMonad
import XMonad.Util.EZConfig

main = xmonad $ def
    { terminal    = "sakura"
    --, focusFollowsMouse
    , borderWidth = 8
    , modMask     = mod4Mask
    --, workspaces
    , normalBorderColor = "#222222"
    , focusedBorderColor = "#222222"
    --, keys = myKeys
    }
    `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
    ]
