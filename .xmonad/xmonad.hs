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
    [ ((0,                      xK_Super_R), nextWS)  -- switch to next workspace
    , ((mod4Mask,               xK_Next),    nextWS)
    , ((mod4Mask,               xK_Down),    nextWS)
    , ((mod4Mask,               xK_Prior),   prevWS)  -- switch to previous workspace
    , ((mod4Mask,               xK_Up),      prevWS)
    , ((mod4Mask .|. shiftMask, xK_Next),    shiftToNext)  -- move window to next workspace
    , ((mod4Mask .|. shiftMask, xK_Down),    shiftToNext)
    , ((mod4Mask .|. shiftMask, xK_Prior),   shiftToPrev)  -- move window to previous workspace
    , ((mod4Mask .|. shiftMask, xK_Up),      shiftToPrev)
    , ((mod4Mask,               xK_Tab),     toggleWS)  -- toggle workspaces
    , ((mod4Mask,               xK_Right),   nextScreen)  -- switch to next screen
    , ((mod4Mask,               xK_Left),    prevScreen)  -- switch to previous screen
    , ((mod4Mask .|. shiftMask, xK_Right),   shiftNextScreen)  -- move window to next screen
    , ((mod4Mask .|. shiftMask, xK_Left),    shiftPrevScreen)  -- move window to previous screen
    , ((mod4Mask .|. shiftMask, xK_l),       spawn "xscreensaver-command -lock")  -- lock screen
    , ((mod4Mask .|. shiftMask, xK_p),       spawn "scrot -e 'mv $f ~/Pictures/'")  -- take screenshot
    , ((0,                      0xFF61),     spawn "scrot -e 'mv $f ~/Pictures/'")
    , ((0,                      0x1008FF11), spawn "amixer -q sset Master 2%-")  -- volume down
    , ((0,                      0x1008FF13), spawn "amixer -q sset Master 2%+")  -- volume up
    , ((0,                      0x1008FF12), spawn "amixer set Master toggle")  -- mute
    ]
