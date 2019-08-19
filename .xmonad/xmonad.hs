import XMonad
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog

main = xmonad =<< statusBar "dzheader" dzenPP toggleStrutsKey myConfig

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myConfig = def
    { terminal = "sakura"
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , borderWidth = 1
    , modMask = mod4Mask
    , normalBorderColor = "#dddddd"
    , focusedBorderColor = "#222222"
    , layoutHook = spacingRaw False (Border 18 12 12 12) True (Border 6 6 6 6) True $ layoutHook def
    --, layoutHook = layoutHook def  -- uncomment this and comment above line to refresh layout
    --, startupHook
    }
    `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
    ]
