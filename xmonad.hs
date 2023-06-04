import XMonad
import System.Directory
import System.Exit
import System.IO (hPutStrLn)
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.FloatKeys
import XMonad.Actions.Navigation2D

import Data.Maybe (fromJust)
import Data.Monoid
import qualified Data.Map        as M

import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, xcomposite in obs, active window for maim screenshots, etc.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.Grid
import XMonad.Layout.Dwindle
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiColumns
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableThreeColumns

import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??), Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders (lessBorders, Ambiguity (OnlyScreenFloat))
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing

import XMonad.Prompt.Pass
import XMonad.Prompt.Ssh

import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

import Themes.Base16

myScript = "$HOME/.local/bin/"
myDMScript = "$HOME/.local/bin/dm-scripts/"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

main :: IO ()
main = xmonad
     . withNavigation2DConfig myNavigation2DConfig
     . docks
     . ewmhFullscreen
     . ewmh
     -- . withSB mySB
     -- . withSB (mySB0 <> mySB1)
     . withSB mySB0
     $ myConfig

myStartupHook = do
    spawnOnce (myScript ++ "auto-start.sh")
    spawn ("$XDG_CONFIG_HOME/trayer/trayer.sh")

    -- -- Manage Workspaces
    -- screenWorkspace 1 >>= flip whenJust (windows . W.view) -- focus the second screen
    -- windows $ W.greedyView "\xf080"                        -- swap second screen to different workspace
    -- screenWorkspace 0 >>= flip whenJust (windows . W.view) -- focus the first screen again

myManageHook = composeAll
    -- General Rules
    [ className =? "confirm"        --> doCenterFloat
    , className =? "file_progress"  --> doCenterFloat
    , className =? "dialog"         --> doCenterFloat
    -- , className =? "dialog"         --> (customFloating $ myFloatingWindow)
    , className =? "download"       --> doCenterFloat
    , className =? "error"          --> doCenterFloat
    , className =? "notification"   --> doCenterFloat
    , className =? "splash"         --> doCenterFloat
    , className =? "toolbar"        --> doCenterFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen                  --> doFullFloat

    -- Floating Apps
    , className =? "MPlayer"        --> doCenterFloat
    , className =? "Gimp"           --> doCenterFloat
    , className =? "mpv"            --> doCenterFloat
    , title     =? "Steam - News"   --> doCenterFloat

    -- Workspace 1 - Internet
    , className =? "firefox"                        --> doShift ( myWorkspaces !! 0 )
    , className =? "Tor Browser"                    --> doShift ( myWorkspaces !! 0 )
    , className =? "Chromium"                       --> doShift ( myWorkspaces !! 0 )
    , className =? "Google-chrome"                  --> doShift ( myWorkspaces !! 0 )
    , className =? "Brave-browser"                  --> doShift ( myWorkspaces !! 0 )
    , className =? "vivaldi-stable"                 --> doShift ( myWorkspaces !! 0 )
    , className =? "qutebrowser"                    --> doShift ( myWorkspaces !! 0 )
    , className =? "nyxt"                           --> doShift ( myWorkspaces !! 0 )

    -- Workspace 2 - Gaming
    , className =? "Wine"                           --> doShift ( myWorkspaces !! 1 )
    , className =? "dolphin-emu"                    --> doShift ( myWorkspaces !! 1 )
    , className =? "Lutris"                         --> doShift ( myWorkspaces !! 1 )
    , className =? "Citra"                          --> doShift ( myWorkspaces !! 1 )
    , className =? "SuperTuxKart"                   --> doShift ( myWorkspaces !! 1 )
    , className =? "steam"                          --> doShift ( myWorkspaces !! 1 )
    , title     =? "Steam"                          --> doShift ( myWorkspaces !! 1 )
    , className =? "battle.net.exe"                 --> doShift ( myWorkspaces !! 1 )
    , title     =? "Battle.net"                     --> doShift ( myWorkspaces !! 1 )

    -- Workspace 3 - Coding
    , className =? "Emacs"                          --> doShift ( myWorkspaces !! 2 )
    , className =? "Geany"                          --> doShift ( myWorkspaces !! 2 )
    , className =? "Atom"                           --> doShift ( myWorkspaces !! 2 )
    , className =? "Subl3"                          --> doShift ( myWorkspaces !! 2 )
    , className =? "code-oss"                       --> doShift ( myWorkspaces !! 2 )
    , className =? "Oomox"                          --> doShift ( myWorkspaces !! 2 )
    , className =? "Unity"                          --> doShift ( myWorkspaces !! 2 )
    , className =? "UnityHub"                       --> doShift ( myWorkspaces !! 2 )
    , className =? "jetbrains-studio"               --> doShift ( myWorkspaces !! 2 )

    -- Workspace 4 - Computer
    , className =? "dolphin"                        --> doShift ( myWorkspaces !! 3 )
    , className =? "ark"                            --> doShift ( myWorkspaces !! 3 )
    , className =? "Nemo"                           --> doShift ( myWorkspaces !! 3 )
    , className =? "pcmanfm"                        --> doShift ( myWorkspaces !! 3 )
    , className =? "File-roller"                    --> doShift ( myWorkspaces !! 3 )
    , className =? "googledocs"                     --> doShift ( myWorkspaces !! 3 )
    , className =? "keep"                           --> doShift ( myWorkspaces !! 3 )
    , className =? "calendar"                       --> doShift ( myWorkspaces !! 3 )

    -- Workspace 5 - Music
    , className =? "ncmpcpp"                        --> doShift ( myWorkspaces !! 4 )
    , className =? "Spotify"                        --> doShift ( myWorkspaces !! 4 )

    -- Workspace 6 - Graphics
    , className =? "Gimp"                           --> doShift ( myWorkspaces !! 5 )
    , className =? "Inkscape"                       --> doShift ( myWorkspaces !! 5 )
    , className =? "Flowblade"                      --> doShift ( myWorkspaces !! 5 )
    , className =? "digikam"                        --> doShift ( myWorkspaces !! 5 )
    , className =? "obs"                            --> doShift ( myWorkspaces !! 5 )

    -- -- Workspace 7 - Video
    -- , className =? "vlc"                            --> doShift ( myWorkspaces !! 6 )
    -- , className =? "kdenlive"                       --> doShift ( myWorkspaces !! 6 )
    -- , title     =? "Celluloid"                      --> doShift ( myWorkspaces !! 6 )

    -- Workspace 7 - Chat
    , title     =? "whatsapp-for-linux"             --> doShift ( myWorkspaces !! 6 )
    , title     =? "Slack"                          --> doShift ( myWorkspaces !! 6 )
    , title     =? "discord"                        --> doShift ( myWorkspaces !! 6 )
    , title     =? "signal"                         --> doShift ( myWorkspaces !! 6 )
    , title     =? "Friends List"                   --> doShift ( myWorkspaces !! 6 )

    -- Workspace 8 - Sandbox
    , className =? "Virt-manager"                   --> doShift ( myWorkspaces !! 7 )
    , className =? "VirtualBox Manager"             --> doShift ( myWorkspaces !! 7 )
    , className =? "VirtualBox Machine"             --> doShift ( myWorkspaces !! 7 )
    , className =? "Cypress"                        --> doShift ( myWorkspaces !! 7 )

    -- Workspace 9 - Monitor
    , className =? "btop"                           --> doShift ( myWorkspaces !! 8 )

    ] <+> namedScratchpadManageHook myScratchPads

myConfig = def
        -- simple stuff
        { terminal           = myTerm
        , focusFollowsMouse  = myFocusFollowsMouse
        , clickJustFocuses   = myClickJustFocuses
        , borderWidth        = myBorderWidth
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

        -- key bindings
        , keys               = myLegacyKeys
        , mouseBindings      = myMouseBindings

        -- hooks, layouts
        , manageHook         = myManageHook <+> manageDocks
        , layoutHook         = lessBorders OnlyScreenFloat
                             $ myLayoutHook
        , startupHook        = myStartupHook
    } `additionalKeysP` myKeysP `additionalKeys` myKeys

myWSFont = "<fn=5>"

myPP :: PP
myPP = def
    { ppTitleSanitize   = xmobarStrip
                        . shorten 30
    , ppSep     = "<fc=" ++ base03 ++ "> | </fc>"         -- Separator between widgets
    , ppOrder   = \(ws:l:t:_) -> [ws,l,t]                 -- order of things in xmobar
    , ppCurrent = xmobarColor base07 base03
                . xmobarBorder "Top" base0E 2
                . wrap (myWSFont ++ " ") " </fn>"         -- Current workspace
    , ppUrgent  = xmobarColor base08 ""
                . wrap (myWSFont ++ " ") " </fn>"         -- Urgent workspace
    , ppVisible = xmobarColor base07 ""
                . xmobarBorder "Top" base04 2
                . wrap (myWSFont ++ " ") " </fn>"         -- Visible but not current workspace
    , ppHidden  = xmobarColor base07 ""
                . wrap (myWSFont ++ " ") " </fn>"         -- Hidden workspaces
    , ppHiddenNoWindows = xmobarColor base02 ""
                        . wrap (myWSFont ++ " ") " </fn>" -- Hidden workspaces (no windows)
    }

mySBConfig = pure (filterOutWsPP [scratchpadWorkspaceTag] myPP)

mySB0 = statusBarProp "xmobar -x 0 ~/.config/xmobar/mainScreen.hs"      (mySBConfig)
-- mySB1 = statusBarProp "xmobar -x 1 ~/.config/xmobar/secondaryScreen.hs" (mySBConfig)

-- cli tools
myTerm          = "alacritty"
myCliFiles      = "vifmrun"
myCliMusic      = "ncmpcpp"
myCliText       = "vim"
myCliSysAudio   = "alsamixer"
myCliSysMonitor = "btop"
myCliSysTasks   = "htop"

-- core tools
myWebBrowser    = "qutebrowser"
myIncBrowser    = "qutebrowser --target private-window"
myTorBrowser    = "torbrowser-launcher"
myIde           = "emacsclient -c -a 'emacs'"
myFiles         = "pcmanfm"
mySteam         = "/usr/bin/steam-runtime %U"

-- extra tools
myLauncher      = "rofi -show drun"
myPassManager   = "rofi-pass"
myVirtManager   = "virt-manager"
myTorrent       = "transmission-gtk"
myCalculator    = "gnome-calculator"
myAnki          = "anki"

-- graphics tools
myPhotoLibrary  = "digikam"
myImgEditor     = "gimp"
myVctEditor     = "inkscape"
myVidEditor     = "kdenlive"

-- chat apps
myWhatsApp      = "whatsapp-for-linux"
myDiscord       = "discord"

-- system tools
mySysPower      = "xfce4-power-manager-settings"
mySysNetwork    = "nm-connection-editor"
mySysBluetooth  = "blueman-manager"

myBarSize = 24
myBorderWidth = 5
                
myGap i = spacingWithEdge i
myGapSize = 7

myNormalBorderColor  = base03 -- gray
myFocusedBorderColor = base0E -- accent

myWorkspaces  = [ "\xf0ac" -- Internet -- I like f268 better
                , "\xf11b" -- Gaming -- I like f1b6 better
                , "\xf11c" -- Coding
                , "\xf07b" -- Computer
                , "\xf025" -- Music
                , "\xf030" -- Graphics
                , "\xf7cd" -- Chat
                , "\xf5fd" -- Sandbox
                , "\xf080" -- Monitor
                ]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myFloatingWindow    = W.RationalRect left_margin top_margin width height
    where
        width       = 0.5
        height      = 0.7
        left_margin = (1.0 - width)/2
        top_margin  = (1.0 - height)/2

myScratchpadTerm = W.RationalRect left_margin top_margin width height
    where
        width       = 0.5
        height      = 0.8
        left_margin = (1.0 - width)/2
        top_margin  = (1.0 - height)/2

myScratchpadCalc    = W.RationalRect left_margin top_margin width height
    where
        width       = 0.2
        height      = 0.4
        left_margin = 0.95 - width
        top_margin  = 0.05

setFloating   w = W.float w myFloatingWindow 
unsetFloating w = W.sink w 
toggleFloating  = withFocused $ \w -> do 
                       windows (\s -> if M.member w (W.floating s)
                                      then unsetFloating w s
                                      else setFloating w s)
  
toggleMaximize   = sendMessage (Toggle NBFULL)
toggleMirror     = sendMessage (Toggle MIRROR)
toggleFullScreen = sendMessage (Toggle NBFULL)    >> sendMessage ToggleStruts
toggleZen        = sendMessage (Toggle NOBORDERS) >> sendMessage ToggleStruts >> toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled 
toggleBorders    = sendMessage (Toggle NOBORDERS)
toggleStatusBar  = sendMessage ToggleStruts
toggleGaps       = toggleScreenSpacingEnabled     >> toggleWindowSpacingEnabled

myNavigation2DConfig = def { defaultTiledNavigation = sideNavigation }

myScratchPads :: [NamedScratchpad]
myScratchPads  = [ NS "terminal"    spawnTerm        findTerm        (customFloating $ myScratchpadTerm)
                 , NS "htop"        spawnHtop        findHtop        (customFloating $ myScratchpadTerm)
                 , NS "files"       spawnCliFiles    findCliFiles    (customFloating $ myScratchpadTerm)
                 , NS "music"       spawnMusic       findMusic       (customFloating $ myScratchpadTerm)
                 , NS "calc"        spawnCalc        findCalc        (customFloating $ myScratchpadCalc)
                 , NS "virtmanager" spawnVirtManager findVirtManager doCenterFloat
                 , NS "torrent"     spawnTorrent     findTorrent     doCenterFloat
                 , NS "whatsapp"    spawnWhatsApp    findWhatsApp    doCenterFloat
                 , NS "discord"     spawnDiscord     findDiscord     doCenterFloat
                 , NS "anki"        spawnAnki        findAnki        doCenterFloat
                 ]
  
  where
    spawnTerm        = myTerm ++ " --class sp-term"
    spawnHtop        = myTerm ++ " --class sp-htop -e"  ++ myCliSysTasks
    spawnCliFiles    = myTerm ++ " --class sp-files -e" ++ myCliFiles
    spawnMusic       = myTerm ++ " --class sp-music -e" ++ myCliMusic
    spawnVirtManager = myVirtManager
    spawnTorrent     = myTorrent
    spawnCalc        = myCalculator
    spawnWhatsApp    = myWhatsApp
    spawnDiscord     = myDiscord
    spawnAnki        = myAnki
    
    findTerm         = className =? "sp-term"
    findHtop         = className =? "sp-htop"
    findCliFiles     = className =? "sp-files"
    findMusic        = className =? "sp-music"
    findVirtManager  = title     =? "Virtual Machine Manager"
    findTorrent      = className =? "transmission-gtk"
    findCalc         = className =? "gnome-calculator"
    findWhatsApp     = className =? "Whatsapp-for-linux"
    findDiscord      = className =? "discord"
    findAnki         = className =? "Anki"

tall    = renamed [Replace "tall"]   -- default tiling algorithm partitions the screen into two panes
        $ myGap myGapSize
        $ ResizableTall
          1      --- The default number of windows in the master pane
          0.03   --- Percent of screen to increment by when resizing panes
          (0.5)  --- Default proportion of screen occupied by master pane
          []
mirror  = renamed [Replace "mirror"] -- tall layout rotated 90 degrees
        $ Mirror tall
grid    = renamed [Replace "grid"]   -- just a grid layout
        $ myGap myGapSize
        $ Grid
columns = renamed [Replace "columns"]   -- just a grid layout
        $ myGap myGapSize
        $ ResizableThreeCol
          1      --- Default value for all following columns.
          0.03   --- Percent of screen to increment by when resizing panes
          (1/3)  --- Initial size of master area, or column area if the size is negative.
          []
spiral  = renamed [Replace "spiral"]
        $ myGap myGapSize
        $ Spiral
          R      --- First split direction
          CW     --- First split chirality
          1      --- Size ratio between rectangle allocated to current window and rectangle allocated to remaining windows
          1.03   --- Factor by which the size ratio is changed in response to Expand or Shrink messages
dwindle = renamed [Replace "dwindle"]
        $ myGap myGapSize
        $ Dwindle
          R      --- First split direction
          CW     --- First split chirality
          1      --- Size ratio between rectangle allocated to current window and rectangle allocated to remaining windows
          1.03   --- Factor by which the size ratio is changed in response to Expand or Shrink messages
full    = renamed [Replace "full"]
        $ myGap myGapSize
        $ Full

myLayoutHook   = avoidStruts
               $ mkToggle (NBFULL ?? EOT)
               $ mkToggle (NOBORDERS ?? EOT)
               $ mkToggle (single MIRROR)
               $ myLayouts
  where
    myLayouts = columns
            ||| tall
            ||| spiral
            ||| full

myModMask = mod4Mask

myKeysP :: [(String, X ())]

myKeysP =
    [ ("M-C-d", spawn ("eww update debug=" ++ "xmonad") ) -- Debugging

    , ("M-C-r"     , spawn "xmonad --recompile; xmonad --restart" ) -- Restart XMonad
    , ("M-C-q"     , io (exitWith ExitSuccess)                    ) -- Quit XMonad

    -- Extra modifier keys were already added to Xmonad-contrib. Waiting for the new version to be released
    , ("S-<Alt_R>" , spawn (myScript ++ "toggle-lang.sh")              ) -- Language Switching

    , ("M-t z"     , toggleZen                                    ) -- Toggle Zen Mode
    , ("M-t g"     , toggleGaps                                   ) -- Toggle Gaps
    , ("M-t b"     , toggleBorders                                ) -- Toggle Window Borders
    , ("M-t s"     , toggleStatusBar                              ) -- Ignore the statusbar
    , ("M-t k"     , spawn (myDMScript ++ "dm-keys toggle")       ) -- Toggle Key Grabber

    , ("M-q"       , kill                          ) -- Close focused Window
    , ("M-<F11>"   , toggleFullScreen              ) -- Toggle Fullscreen
    , ("M-S-f"     , toggleFullScreen              ) -- Toggle Fullscreen
    , ("M-m"       , toggleMaximize                ) -- Toggle Maximize
    , ("M-f"       , toggleFloating                ) -- Toggle Floating

    , ("M-/"       , switchLayer                   ) -- Switch navigation layer (Tiled vs Floating screens)
    , ("M1-<Tab>"  , windows W.focusDown           ) -- Move focus to next Window
    , ("M1-S-<Tab>", windows W.focusUp             ) -- Move focus to prev Window
    , ("M-h"       , windowGo L False              ) -- Move focus to left Window
    , ("M-j"       , windowGo D False              ) -- Move focus to below Window
    , ("M-k"       , windowGo U False              ) -- Move focus to above Window
    , ("M-l"       , windowGo R False              ) -- Move focus to right Window

    , ("M-S-h"     , windowSwap L False            ) -- Swap focused Window with left Window
    , ("M-S-j"     , windowSwap D False            ) -- Swap focused Window with below Window
    , ("M-S-k"     , windowSwap U False            ) -- Swap focused Window with above Window
    , ("M-S-l"     , windowSwap R False            ) -- Swap focused Window with right Window

    , ("M-C-h"     , sendMessage Shrink            ) -- Grow focused Window left
    , ("M-C-j"     , sendMessage MirrorShrink      ) -- Grow focused Window down
    , ("M-C-k"     , sendMessage MirrorExpand      ) -- Grow focused Window up
    , ("M-C-l"     , sendMessage Expand            ) -- Grow focused Window right

    -- , ("M-S-h"     , withFocused (keysMoveWindow (-10,0) )       ) -- Move floating Window left
    -- , ("M-S-l"     , withFocused (keysMoveWindow (10,0) )       ) -- Move floating Window right

    , ("M-M1-j"    , sendMessage (IncMasterN (-1)) ) -- Decrease number of Master Windows
    , ("M-M1-k"    , sendMessage (IncMasterN 1)    ) -- Increase number of Master Windows

    , ("M-,"    , screenGo L False       ) -- Move focus to left Screen
    , ("M-."    , screenGo R False       ) -- Move focus to right Screen

    , ("M-S-,"  , windowToScreen L False ) -- Move focused Window to the left Screen
    , ("M-S-."  , windowToScreen R False ) -- Move focused Window to the right Screen

    , ("M-C-<Tab>"  , screenSwap R True  ) -- Swap active Screen with the next Screen
    , ("M-C-S-h"    , screenSwap L False ) -- Swap active Screen with the left Screen
    , ("M-C-S-j"    , screenSwap D False ) -- Swap active Screen with the below Screen
    , ("M-C-S-k"    , screenSwap U False ) -- Swap active Screen with the above Screen
    , ("M-C-S-l"    , screenSwap R False ) -- Swap active Screen with the right Screen

    , ("M-<Space>"   , sendMessage NextLayout            ) -- Switch Layouts
    , ("M-S-<Space>" , sendMessage FirstLayout           ) -- Switch to default Layout
    , ("M-S-m"       , toggleMirror                      ) -- Mirror Layout
    , ("M-="         , refresh                           ) -- Resize viewed windows to the correct size

    , ("M-<Tab>", toggleWS ) -- Toggle Workspace

    , ("M-`"           , namedScratchpadAction myScratchPads "terminal"    ) -- Terminal Scratchpad
    , ("M-e"           , namedScratchpadAction myScratchPads "files"       ) -- File Manager Scratchpad
    , ("C-M1-<Delete>" , namedScratchpadAction myScratchPads "htop"        ) -- Htop Scratchpad

    , ("M-s m"         , namedScratchpadAction myScratchPads "music"       ) -- Music Scratchpad
    , ("M-s t"         , namedScratchpadAction myScratchPads "torrent"     ) -- Torrent Scratchpad
    , ("M-s a"         , namedScratchpadAction myScratchPads "anki"        ) -- Anki Scratchpad
    , ("M-s v"         , namedScratchpadAction myScratchPads "virtmanager" ) -- VirtManager Scratchpad
    , ("M-s w"         , namedScratchpadAction myScratchPads "whatsapp"    ) -- WhatsApp Scratchpad
    , ("M-s d"         , namedScratchpadAction myScratchPads "discord"     ) -- Discord Scratchpad
    , ("M-s c"         , namedScratchpadAction myScratchPads "calc"        ) -- Calculator Scratchpad
    , ("M-s h"         , namedScratchpadAction myScratchPads "htop"        ) -- Htop Scratchpad

    , ("<XF86AudioRaiseVolume>"  , spawn (myScript ++ "set-volume.sh + 2") ) -- Increase System Volume
    , ("<XF86AudioLowerVolume>"  , spawn (myScript ++ "set-volume.sh - 2") ) -- Decrease System Volume
    , ("<XF86AudioMute>"         , spawn (myScript ++ "toggle-mute.sh"   ) ) -- Mute
    , ("C-<XF86AudioRaiseVolume>", spawn "mpc volume +2"                 ) -- Increase Player Volume
    , ("C-<XF86AudioLowerVolume>", spawn "mpc volume -2"                 ) -- Decrease Player Volume
    , ("<XF86AudioPrev>"         , spawn "mpc prev"                      ) -- Prev Song
    , ("<XF86AudioNext>"         , spawn "mpc next"                      ) -- Next Song
    , ("<XF86AudioPlay>"         , spawn "mpc toggle"                    ) -- Play/Pause Music
    , ("<XF86AudioStop>"         , spawn "mpc stop"                      ) -- Stop Music

    , ("M-d M-d" , spawn (myDMScript ++ "dm-master"     )) -- DM Master
    , ("M-d w"   , spawn (myDMScript ++ "dm-wallpaper"  )) -- DM Wallpaper
    , ("M-d r"   , spawn (myDMScript ++ "dm-record"     )) -- DM Record
    , ("M-d p"   , spawn (myDMScript ++ "dm-power"      )) -- DM Power
    , ("M-d t"   , spawn (myDMScript ++ "dm-theme"      )) -- DM Theme
    , ("M-d s"   , spawn (myDMScript ++ "dm-screenshot" )) -- DM Screenshot
    , ("M-d b"   , spawn (myDMScript ++ "dm-bookman"    )) -- DM Bookman
    , ("M-d n"   , spawn (myDMScript ++ "dm-notify"     )) -- DM Notify
    , ("M-d \\"  , spawn (myDMScript ++ "dm-notify"     )) -- DM Notify
    , ("M-d k"   , spawn (myDMScript ++ "dm-keys"       )) -- DM Keys

    , ("M1-<F4>" , spawn (myDMScript ++ "dm-power"         ) ) -- Logout Menu
    , ("M-z l"   , spawn (myDMScript ++ "dm-power lock"    ) ) -- Lock Screen
    , ("M-z s"   , spawn (myDMScript ++ "dm-power suspend" ) ) -- Suspend System
    , ("M-z p"   , spawn (myDMScript ++ "dm-power poweroff") ) -- Shutdown System
    , ("M-z r"   , spawn (myDMScript ++ "dm-power reboot"  ) ) -- Reboot System
    , ("M-z w"   , spawn (myDMScript ++ "dm-power windows" ) ) -- Reboot to Windows
    , ("M-z z"   , spawn (myDMScript ++ "dm-power suspend" ) ) -- Suspend System
    , ("M-z M-z" , spawn (myDMScript ++ "dm-power suspend" ) ) -- Suspend System

    , ("<Print>"    , spawn (myDMScript ++ "dm-screenshot screen") ) -- Fullscreen Screenshot
    , ("M-S-<Print>", spawn (myDMScript ++ "dm-screenshot area"  ) ) -- Selection Area Screenshot
    , ("M1-<Print>" , spawn (myDMScript ++ "dm-screenshot window") ) -- Active Window Screenshot
    , ("M-<Print>"  , spawn (myDMScript ++ "dm-screenshot full"  ) ) -- Full Desktop Screenshot

    , ("M-\\ \\"   , spawn (myDMScript ++ "dm-notify recent" ) ) -- Show most recent Notification
    , ("M-\\ M-\\" , spawn (myDMScript ++ "dm-notify recent" ) ) -- Show most recent Notification
    , ("M-\\ |"    , spawn (myDMScript ++ "dm-notify recents") ) -- Show few recent Notifications
    , ("M-\\ r"    , spawn (myDMScript ++ "dm-notify recents") ) -- Show few recent Notifications
    , ("M-\\ S-c"  , spawn (myDMScript ++ "dm-notify clear"  ) ) -- Clear all Notifications
    , ("M-\\ c"    , spawn (myDMScript ++ "dm-notify close"  ) ) -- Clear last Notification
    , ("M-\\ a"    , spawn (myDMScript ++ "dm-notify context") ) -- Open last Notification

    , ("C-M1-t"    , spawn (myTerm)               ) -- Launch Terminal
    , ("M-<Return>", spawn (myTerm)               ) -- Launch Terminal
    , ("M-c"       , spawn (myIde)                ) -- Launch IDE
    , ("M-S-e"     , spawn (myFiles)              ) -- Launch File Manager
    , ("M-b"       , spawn (myWebBrowser)         ) -- Launch Web Browser
    , ("M-i"       , spawn (myIncBrowser)         ) -- Launch Web Browser in Incognito Mode
    , ("M-p"       , spawn (myPassManager)        ) -- Autofill Passwords
    , ("M-r"       , spawn (myLauncher)           ) -- Launch Launcher
    , ("M-S-r"     , spawn "dmenu_run"            ) -- Launch dmenu

    -- Primary
    , ("M-o t"     , spawn (myTorBrowser)         ) -- Launch Tor Browser
    , ("M-o m"     , spawn (myTerm ++ " --class ncmpcpp -e " ++ myCliMusic) ) -- Launch Music Player
    , ("M-o s"     , spawn (mySteam)              ) -- Launch Steam

    -- Secondary
    , ("C-M1-o t"  , spawn (myTerm ++ myCliText)  ) -- Launch Text Editor
    , ("C-M1-o p"  , spawn (myPhotoLibrary)       ) -- Launch Photo Library
    , ("C-M1-o g"  , spawn (myImgEditor)          ) -- Launch Image Editor
    , ("C-M1-o r"  , spawn (myVctEditor)          ) -- Launch Vector Editor
    , ("C-M1-o v"  , spawn (myVidEditor)          ) -- Launch Video Editor
  ]

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
    [ ((shiftMask, xK_Alt_L), spawn (myScript ++ "toggle-lang.sh") ) -- Language Switching

    -- Push window back into tiling
    -- , ((mod4Mask,               xK_t     ), withFocused $ windows . W.sink)

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- , ((mod4Mask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    -- , ((mod4Mask          , xK_b     ), sendMessage ToggleStruts)
    ]

myLegacyKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{F1,F2,F3}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{F1,F2,F3}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_F1, xK_F2, xK_F3] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
