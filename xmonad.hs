import XMonad
import Data.Monoid
import System.Exit
import XMonad.Util.EZConfig(additionalKeysP)
import qualified XMonad.StackSet as W
import XMonad.Actions.Navigation2D
import XMonad.Actions.WorkspaceNames
import qualified Data.Map as M
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadSpawnActionCustom, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers (doRectFloat)
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
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedWindows (getName)
import System.IO 

myStartupHook = startup
startup = do
          spawnOnce "setxkbmap gb"
--          spawnOnce "qutebrowser"
          spawnOnce "mpd"
          spawnOnce "mpd-notifcation"
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

myTerminal          = "st"
myFocusFollowsMouse = True
myBorderWidth       = 2
altMask             = mod1Mask

myWorkspaces = clickable . (map dzenEscape) $ ["1","2","3","4","5","6"]
  where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                        (i,ws) <- zip [1..] l,
                        let n = i ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return ), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p      ), shellPrompt paulXPConfig)
    , ((modm,               xK_c      ), spawn "qutebrowser")
    , ((modm,               xK_o      ), runInTerm "" "passw")
--    , ((modm,               xK_c      ), spawn "chromium")
--    , ((modm,               xK_q      ), spawn "bookmarks")
    , ((modm,               xK_q      ), spawn "bookmarksq")
    , ((modm .|. shiftMask, xK_t      ), runInTerm "" "ranger")
    , ((modm .|. shiftMask, xK_r      ), runInTerm "" "tuir")
    , ((modm .|. shiftMask, xK_m      ), runInTerm "" "neomutt")         
    , ((modm,               xK_Escape ), spawn "shutdown-menu.sh")
    , ((modm .|. shiftMask, xK_n      ), runInTerm "" "ncmpcpp") 
    , ((modm .|. shiftMask, xK_s      ), scratchpad)
    , ((modm .|. shiftMask, xK_a      ), scratchpad1)
    , ((modm .|. shiftMask, xK_c      ), kill)
    , ((modm,               xK_b      ), sendMessage ToggleStruts)
    , ((modm,               xK_Return ), spawn myTerminal)
    , ((modm,               xK_space  ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space  ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n      ), do
              spawn ("date>>"++"/home/paul/downloads/notes")
              appendFilePrompt paulXPConfig "/home/paul/downloads/notes")
    , ((0,               0x1008ff11   ), spawn "volume.sh down")
    , ((0,               0x1008ff13   ), spawn "volume.sh up")
    , ((0,               0x1008ff12   ), spawn "volume.sh mute")
    , ((0,               0x1008ff02   ), spawn "brightness.sh up")
    , ((0,               0x1008ff03   ), spawn "brightness.sh down")
    , ((modm .|. shiftMask,   xK_l    ), sendMessage $ ExpandTowards R)
    , ((modm .|. shiftMask,   xK_h    ), sendMessage $ ExpandTowards L)
    , ((modm .|. shiftMask,   xK_j    ), sendMessage $ ExpandTowards D)
    , ((modm .|. shiftMask,   xK_k    ), sendMessage $ ExpandTowards U)
    , ((modm,                 xK_Down ), windows W.focusDown)
    , ((modm,                 xK_Up   ), windows W.focusUp)
    , ((modm .|. shiftMask,   xK_m      ), windows W.swapMaster)
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
 
    where

     scratchpad  = namedScratchpadAction myScratchPads "term"
     scratchpad1 = namedScratchpadAction myScratchPads "ssh"

myScratchPads = [ NS "ssh"  spawnSsh  findSsh  manageSsh  
                , NS "term" spawnTerm findTerm manageTerm 
                ]

  where

    spawnSsh  = myTerminal ++ " -n scratchpad1"                               
    findSsh   = resource  =? "scratchpad1"                  
    manageSsh = customFloating $ W.RationalRect l t w h 

      where

        h = 0.3      
        w = 0.4      
        t = 0.6 
        l = 0.6   

    spawnTerm  = myTerminal ++ " -n scratchpad"      
    findTerm   = resource  =? "scratchpad"               
    manageTerm = customFloating $ W.RationalRect l t w h

      where
 
        h = 0.3      
        w = 0.4       
        t = 0.2     
        l = 0.6  

paulXPConfig = defaultXPConfig
       {     font        = "xft:Product Sans:size=9"
           , bgColor     = "#3f3f3f"
           , fgColor     = "#f0dfaf"
           , fgHLight    = "#3f3f3f"
           , bgHLight    = "#3f3f3f"
           , promptBorderWidth = 0
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
    , resource  =? "ranger"                    --> doShift (myWorkspaces !! 2)
    , resource  =? "vlc"                       --> doRectFloat (W.RationalRect 0.05 0.05 0.6 0.6)
    , resource  =? "feh"                       --> doFloat
    , resource  =? "mpv"                       --> doFloat
    , resource  =? "gl"                        --> doFloat
    , resource  =? "neomutt"                   --> doShift (myWorkspaces !! 1)
    , resource  =? "desktop_window"            --> doIgnore 
    ] <+> namedScratchpadManageHook myScratchPads

myLogHook h  = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }
myDzenStatus = "dzen2 -w '900' -y '1060' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.conkyrc-weather | dzen2 -y '1060' -x '900' -w '1020' -ta 'r'" ++ myDzenStyle
myDzenConky1 = "conky -c ~/.conkyrc-weather0 | dzen2 -y 0 -x 0 -w '1920' -ta 'm'" ++ myDzenStyle
myDzenStyle  = " -h '20' -fg '#777777' -bg '#3f3f3f' -fn 'Product Sans:size=9'"
myDzenPP = dzenPP
     { ppCurrent = dzenColor "#000000" "#3399ff" . wrap " " " "
     , ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP)
     , ppHidden = dzenColor "#dddddd" "" . wrap " " " "
     , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
     , ppUrgent = dzenColor "#000000" "#ff0000" . wrap " " " "
     , ppSep = " "
     , ppLayout = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
     , ppTitle = dzenColor "#ffffff" ""
                   . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                   " ^ca()^ca()" . shorten 500 . dzenEscape
    }
