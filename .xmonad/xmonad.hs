import XMonad
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS

main = xmonad myConfig

myConfig = def
    { terminal = "mate-terminal"
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , borderWidth = 1
    , modMask = mod4Mask
    , normalBorderColor = "#222222"
    , focusedBorderColor = "#dddddd"
    , layoutHook = spacingRaw False (Border 18 12 12 12) True (Border 6 6 6 6) True $ layoutHook def
    --, layoutHook = layoutHook def  -- uncomment this and comment above line to refresh layout
    , startupHook = spawn "dzheader" >> spawn "dzquotes"
    }
    `additionalKeys`
    [ ((0,                      xK_Super_R), nextWS)
    , ((mod4Mask,               xK_Down),    nextWS)
    , ((mod4Mask,               xK_Up),      prevWS)
    , ((mod4Mask,               xK_Right),   shiftToNext)
    , ((mod4Mask,               xK_Left),    shiftToPrev)
    , ((mod4Mask .|. shiftMask, xK_Down),    nextScreen)
    , ((mod4Mask .|. shiftMask, xK_Up),      prevScreen)
    , ((mod4Mask .|. shiftMask, xK_Right),   shiftNextScreen)
    , ((mod4Mask .|. shiftMask, xK_Left),    shiftPrevScreen)
    , ((mod4Mask,               xK_Tab),     toggleWS)
    , ((mod4Mask .|. shiftMask, xK_l),       spawn "xscreensaver-command -lock")
    , ((mod4Mask .|. shiftMask, xK_p),       spawn "scrot -e 'mv $f ~/Pictures/'")
    , ((0,                      0xFF61),     spawn "scrot -e 'mv $f ~/Pictures/'")
    , ((0,                      0x1008FF11), spawn "amixer -q sset Master 2%-")
    , ((0,                      0x1008FF13), spawn "amixer -q sset Master 2%+")
    , ((0,                      0x1008FF12), spawn "amixer set Master toggle")
    ]
