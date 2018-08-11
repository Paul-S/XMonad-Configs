import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.EZConfig(additionalKeysP)
import qualified XMonad.StackSet as W
import XMonad.Actions.Navigation2D
import XMonad.Actions.WorkspaceNames
import qualified Data.Map        as M
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadSpawnActionCustom, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run (spawnPipe, runInTerm)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Theme
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.AppendFile
import XMonad.Layout.BinarySpacePartition hiding (Swap)
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.EqualSpacing
import XMonad.Layout.Gaps
import XMonad.Layout.Circle
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import Data.Ratio ((%))
import XMonad.Actions.Search
import XMonad.Actions.UpdatePointer
import XMonad.Util.NamedScratchpad
import qualified XMonad.Prompt         as P
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.WorkspaceCompare
import System.IO 

myStartupHook = startup
startup = do
          spawn "setxkbmap gb"
          spawn "xterm -name ranger -e ranger"
          spawn "xterm -name neomutt -e neomutt"
          spawn "qutebrowser"
main = do
        status <- spawnPipe myDzenStatus
        conky  <- spawnPipe myDzenConky 
        conky1 <- spawnPipe myDzenConky1
        xmonad $ withNavigation2DConfig defaultNavigation2DConfig 
               $ withUrgencyHook NoUrgencyHook 
               $ defaultConfig
	   { terminal = myTerminal
           , normalBorderColor  = "#3f3f3f"
           , workspaces         = myWorkspaces
           , focusedBorderColor = "#f0dfaf"
           , logHook            = myLogHook status >> updatePointer (0.95, 0.95) (0, 0)
           , modMask            = mod4Mask
           , layoutHook         = myLayoutHook
           , focusFollowsMouse  = myFocusFollowsMouse
           , manageHook         = myManageHook <+> manageHook defaultConfig
           , handleEventHook    = docksEventHook <+> handleEventHook defaultConfig 
           , startupHook        = myStartupHook
           , keys               = \c -> myKeys c
           }

myTerminal          = "xterm"
myFocusFollowsMouse = True
myBorderWidth       = 2
altMask             = mod1Mask

myWorkspaces = clickable . (map dzenEscape) $ ["1","2","3","4","5"]
  where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                        (i,ws) <- zip [1..] l,
                        let n = i ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return ), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p      ), shellPrompt paulXPConfig)
    , ((modm,               xK_c      ), spawn "qutebrowser")
    , ((modm,               xK_f      ), spawn "firefox-beta-bin")
    , ((modm,               xK_d      ), spawn "bookmarks")
    , ((modm,               xK_q      ), spawn "bookmarksq")
    , ((modm,               xK_w      ), spawn "bookmarksi")
    , ((modm,               xK_t      ), spawn "xterm -name ranger -e ranger")
    , ((modm,               xK_m      ), spawn "xterm -name neomutt -e neomutt")         
    , ((modm,               xK_Escape ), spawn "shutdown-menu.sh")
    , ((modm .|. shiftMask, xK_n      ), spawn "mpd && xterm -name ncmpcpp -e ncmpcpp") 
    , ((modm .|. shiftMask, xK_s      ), scratchPad)
    , ((modm .|. shiftMask, xK_c      ), kill)
    , ((modm,               xK_b      ), sendMessage ToggleStruts)
    , ((modm,               xK_Return ), spawn myTerminal)
    , ((modm,               xK_space  ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space  ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n      ), do
              spawn ("date>>"++"/home/paul/downloads/notes")
              appendFilePrompt paulXPConfig "/home/paul/downloads/notes")
    , ((0,               0x1008ff11   ), spawn "/usr/bin/pulseaudio-ctl down")
    , ((0,               0x1008ff13   ), spawn "/usr/bin/pulseaudio-ctl up")
    , ((0,               0x1008ff12   ), spawn "/usr/bin/pulseaudio-ctl mute")
    , ((modm .|. shiftMask,   xK_l    ), sendMessage $ ExpandTowards R)
    , ((modm .|. shiftMask,   xK_h    ), sendMessage $ ExpandTowards L)
    , ((modm .|. shiftMask,   xK_j    ), sendMessage $ ExpandTowards D)
    , ((modm .|. shiftMask,   xK_k    ), sendMessage $ ExpandTowards U)
    , ((modm,                 xK_Down ), windows W.focusDown)
    , ((modm,                 xK_Up   ), windows W.focusUp)
    , ((modm .|. altMask,   xK_m      ), windows W.swapMaster)
    , ((modm,               xK_s      ), sendMessage $ BSP.Swap)
    , ((modm,               xK_r      ), spawn "xmonad --recompile && xmonad --restart")
    ]

     ++
 
     [((m .|. modm, k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
     
     ++
     
     [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

paulXPConfig = defaultXPConfig
       {     font        = "xft:Product Sans:size=11"
           , bgColor     = "#3f3f3f"
           , fgColor     = "#f0dfaf"
           , fgHLight    = "#3f3f3f"
           , bgHLight    = "#3f3f3f"
           , promptBorderWidth = 2
           , position    = Bottom
           , height      = 20
           , historySize = 10 
           , defaultText = []
       }

myLayoutHook = smartBorders $ avoidStruts $ myLayouts 
myLayouts    = mkToggle (single FULL) (myLayout1 ||| Circle ||| Full)
    where myBSP     = emptyBSP
          myCircle  = renamed [Replace "Circle"]
          myLayout1 = renamed [Replace "Gaps"] $ equalSpacing 36 6 1 1 $ myBSP
          myFull    = renamed [Replace "Full"] $ Full

myManageHook = manageDocks <+> composeAll 
    [ className =? "qutebrowser"               --> doShift (myWorkspaces !! 0)
    , className =? "firefox"                   --> doShift (myWorkspaces !! 4)
    , className =? "Chromium"                  --> doShift (myWorkspaces !! 0)
    , resource  =? "ncmpcpp"                   --> doShift (myWorkspaces !! 1)
    , resource  =? "gedit"                     --> doShift (myWorkspaces !! 5)
    , resource  =? "ranger"                    --> doShift (myWorkspaces !! 2)
    , resource  =? "vlc"                       --> doFloat
    , resource  =? "neomutt"                   --> doShift (myWorkspaces !! 1)
    , resource  =? "desktop_window"            --> doIgnore 
    ] <+> manageScratchPad

manageScratchPad :: ManageHook  
manageScratchPad = scratchpadManageHook (W.RationalRect (2/3) (1/10) (1/3) (1/4))
scratchPad = scratchpadSpawnActionCustom "xterm -name scratchpad"

myLogHook h  = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }
myDzenStatus = "dzen2 -w '500' -y '880' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.conkyrc-weather | dzen2 -y '880' -x '500' -w '1100' -ta 'r'" ++ myDzenStyle
myDzenConky1 = "conky -c ~/.conkyrc-weather0 | dzen2 -y 0 -x 0 -w '1600' -ta 'm'" ++ myDzenStyle
myDzenStyle  = " -h '20' -fg '#777777' -bg '#3f3f3f' -fn 'Product Sans:size=11'"
myDzenPP = dzenPP
     { ppCurrent = dzenColor "#3399ff" "" . wrap " " " "
     , ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP)
     , ppHidden = dzenColor "#dddddd" "" . wrap " " " "
     , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
     , ppUrgent = dzenColor "#ff0000" "" . wrap " " " "
     , ppSep = " "
     , ppLayout = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
     , ppTitle = dzenColor "#ffffff" ""
                   . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                   " ^ca()^ca()" . shorten 50 . dzenEscape
    }
