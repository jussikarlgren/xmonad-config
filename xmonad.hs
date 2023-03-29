import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Loggers -- for xmobar
import XMonad.Actions.SpawnOn





main :: IO ()
main = xmonad
     -- . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey  --  defToggleStrutsKey: M-b
     $ myConfig


myStartupHook :: X ()
myStartupHook = def


myManageHook = composeAll [
         spawnOn "4" "/usr/bin/thunderbird",
         spawnOn "2" "/snap/bin/brave",
         spawnOn "9" "/snap/bin/firefox",
         spawnOn "1" "/usr/bin/nautilus",
         spawnOn "5" "/snap/bin/slack",
         spawnOn "5" "/snap/bin/discord"
         ]



-- /usr/bin/thunderbird
-- /snap/bin/firefox
-- /usr/bin/nautilus
-- /usr/bin/emacs
-- /snap/bin/brave



myConfig = def
     {
      modMask = mod4Mask     -- Rebind Mod to the Windows (aka "super") key
--     , startupHook = myStartupHook
--     , manageHook = manageSpawn myManageHook
     }
     `additionalKeysP`
     -- You can find the names for special keys, like Print or the F-keys,
     -- in the XMonad.Util.EZConfig documentation.
     --
     -- M- for the mod key, S- for shift, C- for control
     -- 
     [ ("M-f", spawn "firefox"),
       ("<Print>", unGrab *> spawn "scrot -s") --  *> operator sequences two functions discarding the output 
     ]


myXmobarPP = def
   {
   ppSep   =  magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
           where
                formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
                formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
                ppWindow :: String -> String
                ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30
            
                blue, lowWhite, magenta, red, white, yellow :: String -> String
                magenta  = xmobarColor "#ff79c6" ""
                blue     = xmobarColor "#bd93f9" ""
                white    = xmobarColor "#f8f8f2" ""
                yellow   = xmobarColor "#f1fa8c" ""
                red      = xmobarColor "#ff5555" ""
                lowWhite = xmobarColor "#bbbbbb" ""
            
{-
-- import XMonad.Util.Run(spawnPipe)
--import System.IO
main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ def  -- dollar sign is syntactic sugar for parenthesising the def expression whatevs
        { manageHook = manageDocks <+> manageHook def
        , layoutHook = avoidStruts  $  layoutHook def
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        ,
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]

-}