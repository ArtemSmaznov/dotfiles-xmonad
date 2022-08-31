import XMonad
import System.Directory
import System.Exit
import System.IO (hPutStrLn)
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.Navigation2D

import Data.Maybe (fromJust)
import Data.Monoid
import qualified Data.Map        as M

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, pad, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, xcomposite in obs, active window for maim screenshots, etc.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.Grid
import XMonad.Layout.Dwindle
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiColumns

import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??), Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders (lessBorders, Ambiguity (OnlyScreenFloat))
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing

import XMonad.Prompt.Pass
import XMonad.Prompt.Ssh

import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

import Colors.Gruvbox

main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmonad/xmobar/mainScreen.hs"
    xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmonad/xmobar/secondaryScreen.hs"
    
    xmonad $ withNavigation2DConfig myNavigation2DConfig 
           $ docks
           $ ewmhFullscreen
           $ ewmh def
        -- simple stuff
        { terminal           = myTerminal
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
        , logHook            = dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
            { ppOutput = \x -> hPutStrLn xmproc0 x -- xmobar on Monitor 1
                            >> hPutStrLn xmproc1 x -- xmobar on Monitor 2

            -- Current workspace
            , ppCurrent          = xmobarColor foreground bg3
                                   . wrap ("<box type=Top width=2 color=" ++ color11 ++ ">") "</box>"
                                   . clickable

            -- Visible but not current workspace
            , ppVisible          = xmobarColor foreground ""
                                   . wrap ("<box type=Top width=2 color=" ++ color14 ++ ">") "</box>"
                                   . clickable
            -- Hidden workspaces
            , ppHidden           = xmobarColor foreground ""
                                   . clickable

            -- Hidden workspaces (no windows)
            , ppHiddenNoWindows  = xmobarColor bg2 ""
                                   . clickable

            -- Urgent workspace
            , ppUrgent           = xmobarColor color09 ""
                                   . clickable

            -- Title of active window
            , ppTitle            = xmobarColor foreground ""
                                   . shorten 60

            -- Separator between widgets
            , ppSep              = "<fc=" ++ bg4 ++ "> | </fc>"

            -- order of things in xmobar
            , ppOrder            = \(l:ws:t:_) -> [l,ws,t]
            }

    } `additionalKeysP` myKeysP `additionalKeys` myKeys

myStartupHook = do
    spawnOnce "$HOME/.config/autostart-scripts/autostart.sh"

    -- System Tray
    spawn "killall trayer"  -- kill current trayer on each restart
    spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 " ++ colorTrayer ++ " --height " ++ show myBarSize ++ "")

myManageHook = composeAll
    -- General Rules
    [ className =? "confirm"        --> doFloat
    , className =? "file_progress"  --> doFloat
    , className =? "dialog"         --> (customFloating $ myFloatingWindow)
    , className =? "download"       --> doFloat
    , className =? "error"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "notification"   --> doFloat
    , className =? "splash"         --> doFloat
    , className =? "toolbar"        --> doFloat
    , className =? "mpv"            --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen                  --> doFullFloat

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
    , className =? "Steam"                          --> doShift ( myWorkspaces !! 1 )
    , className =? "battle.net.exe"                 --> doShift ( myWorkspaces !! 1 )
    , title     =? "Steam"                          --> doShift ( myWorkspaces !! 1 )
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
    , className =? "Spotify"                        --> doShift ( myWorkspaces !! 4 )
    , className =? "youtubemusic-nativefier-040164" --> doShift ( myWorkspaces !! 4 )
                    
    -- Workspace 6 - Graphics
    , className =? "Gimp"                           --> doShift ( myWorkspaces !! 5 )
    , className =? "Gimp-2.10"                      --> doShift ( myWorkspaces !! 5 )
    , className =? "Gimp"                           --> doShift ( myWorkspaces !! 5 )
    , className =? "Inkscape"                       --> doShift ( myWorkspaces !! 5 )
    , className =? "Flowblade"                      --> doShift ( myWorkspaces !! 5 )
    , className =? "digikam"                        --> doShift ( myWorkspaces !! 5 )
    
    -- Workspace 7 - Video
    , className =? "vlc"                            --> doShift ( myWorkspaces !! 6 )
    , className =? "obs"                            --> doShift ( myWorkspaces !! 6 )
    , className =? "kdenlive"                       --> doShift ( myWorkspaces !! 6 )
    , title     =? "Celluloid"                      --> doShift ( myWorkspaces !! 6 )
    
    -- Workspace 8 - Chat
    , title     =? "whatsapp-for-linux"             --> doShift ( myWorkspaces !! 7 )
    , title     =? "Slack"                          --> doShift ( myWorkspaces !! 7 )
    , title     =? "discord"                        --> doShift ( myWorkspaces !! 7 )
    , title     =? "signal"                         --> doShift ( myWorkspaces !! 7 )
      
    -- Workspace 9 - Sandbox
    , className =? "Virt-manager"                   --> doShift ( myWorkspaces !! 8 )
    , className =? "VirtualBox Manager"             --> doShift ( myWorkspaces !! 8 )
    , className =? "VirtualBox Machine"             --> doShift ( myWorkspaces !! 8 )
    , className =? "Cypress"                        --> doShift ( myWorkspaces !! 8 )
    ] <+> namedScratchpadManageHook myScratchPads

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myTerminal         = "alacritty"
myWebBrowser       = "qutebrowser"
myIncognitoBrowser = "qutebrowser --target private-window"
myTorBrowser       = "torbrowser-launcher"
myGame             = "/usr/bin/steam-runtime %U"
myFileManager      = "pcmanfm"
myCliFileManager   = "vifmrun"
myTextEditor       = myTerminal ++ " -e vim"
myIde              = "emacsclient -c -a 'emacs'"
myMusicPlayer      = myTerminal ++ " -e ncmpcpp"
myVideoPlayer      = "mpv"
myVideoEditor      = "kdenlive"
myPhotoLibrary     = "digikam"
myImageEditor      = "gimp"
myVectorEditor     = "inkscape"
myTorrentClient    = "transmission-gtk"
myCalculator       = "gnome-calculator"
myVm               = "virt-manager"

myWhatsApp         = "whatsapp-for-linux"
myDiscord          = "discord"

myLauncher         = "rofi -show drun"
myPasswordManager  = "rofi-pass"
myNetworkManager   = "nm-connection-editor"
myBluetoothManager = "blueman-manager"
myPowerManager     = "xfce4-power-manager-settings"
myAudioManager     = myTerminal ++ " -e alsamixer"

myBarSize = 24
myBorderWidth = 4
                
myGap i = spacingWithEdge i
myGapSize = 7

myNormalBorderColor  = color08
myFocusedBorderColor = color09

myFloatingWindow    = W.RationalRect left_margin top_margin width height
    where
        width       = 0.7
        height      = 0.7
        left_margin = (1.0 - width)/2
        top_margin  = (1.0 - height)/2

myScratchpadWindow  = W.RationalRect left_margin top_margin width height
    where
        width       = 0.8
        height      = 0.8
        left_margin = (1.0 - width)/2
        top_margin  = (1.0 - height)/2

myScratchpadCalc    = W.RationalRect left_margin top_margin width height
    where
        width       = 0.2
        height      = 0.4
        left_margin = 0.95 - width
        top_margin  = 0.05

myScratchpadChat    = W.RationalRect left_margin top_margin width height
    where
        width       = 0.5
        height      = 0.9
        left_margin = (1.0 - width)/2
        top_margin  = (1.0 - height)/2

myScratchpadAnki    = W.RationalRect left_margin top_margin width height
    where
        width       = 0.4
        height      = 0.9
        left_margin = (1.0 - width)/2
        top_margin  = (1.0 - height)/2

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

myWorkspaces  = [ "<fn=2> \xf268 </fn>" -- Internet
                , "<fn=2> \xf1b6 </fn>" -- Gaming
                , "<fn=1> \xf11c </fn>" -- Coding
                , "<fn=1> \xf07b </fn>" -- Computer
                , "<fn=1> \xf025 </fn>" -- Music
                , "<fn=1> \xf030 </fn>" -- Graphics
                , "<fn=1> \xf03d </fn>" -- Video
                , "<fn=1> \xf7cd </fn>" -- Chat
                , "<fn=2> \xf395 </fn>" -- Sandbox
                ]
  
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myScratchPads :: [NamedScratchpad]
myScratchPads  = [ NS "terminal"    spawnTerm        findTerm        (customFloating $ myScratchpadWindow)
                 , NS "htop"        spawnHtop        findHtop        (customFloating $ myScratchpadWindow)
                 , NS "cliFiles"    spawnCliFiles    findCliFiles    (customFloating $ myScratchpadWindow)
                 , NS "music"       spawnMusic       findMusic       (customFloating $ myScratchpadWindow)
                 , NS "virtmanager" spawnVirtManager findVirtManager (customFloating $ myScratchpadWindow)
                 , NS "torrent"     spawnTorrent     findTorrent     (customFloating $ myScratchpadWindow)
                 , NS "calc"        spawnCalc        findCalc        (customFloating $ myScratchpadCalc)
                 , NS "whatsapp"    spawnWhatsApp    findWhatsApp    (customFloating $ myScratchpadChat)
                 , NS "discord"     spawnDiscord     findDiscord     (customFloating $ myScratchpadChat)
                 , NS "anki"        spawnAnki        findAnki        (customFloating $ myScratchpadAnki)
                 ]
  
  where
    spawnTerm        = myTerminal ++ " -t scratchpad"
    spawnHtop        = myTerminal ++ " -t htop -e htop"
    spawnCliFiles    = myTerminal ++ " -t cliFiles -e " ++ myCliFileManager
    spawnMusic       = myTerminal ++ " --class ncmpcpp,music -e ncmpcpp"
    spawnVirtManager = myVm
    spawnTorrent     = myTorrentClient
    spawnCalc        = myCalculator
    spawnWhatsApp    = myWhatsApp
    spawnDiscord     = myDiscord
    spawnAnki        = "anki"
    
    findTerm         = title     =? "scratchpad"
    findHtop         = title     =? "htop"
    findCliFiles     = title     =? "cliFiles"
    findMusic        = className =? "music"
    findVirtManager  = title     =? "Virtual Machine Manager"
    findTorrent      = className =? "Transmission-gtk"
    findCalc         = className =? "Gnome-calculator"
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
        $ multiCol
          [1]    --- Windows in each column, starting with master. Set to 0 to catch the rest.
          1      --- Default value for all following columns.
          0.03   --- Percent of screen to increment by when resizing panes
          (-0.5) --- Initial size of master area, or column area if the size is negative.
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
    myLayouts = tall 
            ||| columns
            ||| spiral
            ||| full

myModMask = mod4Mask

myKeysP :: [(String, X ())]

myKeysP =
    [ ("M-C-d", sshPrompt def ) -- Debugging

    , ("M-C-r"     , spawn "xmonad --recompile; xmonad --restart"       ) -- Restart XMonad
    , ("M-C-q"     , io (exitWith ExitSuccess)                          ) -- Quit XMonad

    -- Extra modifier keys were already added to Xmonad-contrib. Waiting for the new version to be released
    , ("S-<Alt_R>" , spawn "$HOME/.local/bin/dm-scripts/dm-lang"        ) -- Language Switching

    , ("M-t z"     , toggleZen                                          ) -- Toggle Zen Mode
    , ("M-t g"     , toggleGaps                                         ) -- Toggle Gaps
    , ("M-t b"     , toggleBorders                                      ) -- Toggle Window Borders
    , ("M-t s"     , toggleStatusBar                                    ) -- Ignore the statusbar
    , ("M-t k"     , spawn "$HOME/.local/bin/dm-scripts/dm-keys toggle" ) -- Toggle Key Grabber

    , ("M-q"       , kill                          ) -- Close focused Window
    , ("M-<F11>"   , toggleFullScreen              ) -- Toggles Fullscreen
    , ("M-S-f"     , toggleFullScreen              ) -- Toggles Fullscreen
    , ("M-m"       , toggleMaximize                ) -- Toggle Maximize
    , ("M-f"       , toggleFloating                ) -- Toggle Floating

    , ("M-/"     , switchLayer                   ) -- Switch navigation layer (Tiled vs Floating screens)
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

    , ("M-M1-j"    , sendMessage (IncMasterN (-1)) ) -- Decrease number of Master Windows
    , ("M-M1-k"    , sendMessage (IncMasterN 1)    ) -- Increase number of Master Windows

    , ("M-,"    , screenGo L False       ) -- Move focus to left Screen
    , ("M-."    , screenGo R False       ) -- Move focus to right Screen

    , ("M-S-,"  , windowToScreen L False ) -- Move focused Window to the left Screen
    , ("M-S-."  , windowToScreen R False ) -- Move focused Window to the right Screen

    , ("M-C-<Tab>", screenSwap R True  ) -- Swap active Screen with the next Screen
    , ("M-C-S-h"    , screenSwap L False ) -- Swap active Screen with the left Screen
    , ("M-C-S-j"    , screenSwap D False ) -- Swap active Screen with the below Screen
    , ("M-C-S-k"    , screenSwap U False ) -- Swap active Screen with the above Screen
    , ("M-C-S-l"    , screenSwap R False ) -- Swap active Screen with the right Screen

    , ("M-<Space>"   , sendMessage NextLayout            ) -- Switch Layouts
    , ("M-S-<Space>" , sendMessage FirstLayout           ) -- Switch to default Layout
    , ("M-S-m"       , toggleMirror                      ) -- Mirror Layout
    , ("M-="         , refresh                           ) -- Resize viewed windows to the correct size

    , ("M-<Tab>"       , toggleWS ) -- Toggle Workspace

    , ("M-`"           , namedScratchpadAction myScratchPads "terminal"    )
    , ("M-e"           , namedScratchpadAction myScratchPads "cliFiles"    )
    , ("C-M1-<Delete>" , namedScratchpadAction myScratchPads "htop"        )
    , ("M-s h"         , namedScratchpadAction myScratchPads "htop"        )
    , ("M-s m"         , namedScratchpadAction myScratchPads "music"       )
    , ("M-s c"         , namedScratchpadAction myScratchPads "calc"        )
    , ("M-s w"         , namedScratchpadAction myScratchPads "whatsapp"    )
    , ("M-s d"         , namedScratchpadAction myScratchPads "discord"     )
    , ("M-s v"         , namedScratchpadAction myScratchPads "virtmanager" )
    , ("M-s t"         , namedScratchpadAction myScratchPads "torrent"     )
    , ("M-s a"         , namedScratchpadAction myScratchPads "anki"        )

    , ("<XF86AudioRaiseVolume>"  , spawn "amixer set Master 2%+ unmute" )
    , ("<XF86AudioLowerVolume>"  , spawn "amixer set Master 2%- unmute" )
    , ("<XF86AudioMute>"         , spawn "amixer set Master toggle"     )
    , ("C-<XF86AudioRaiseVolume>", spawn "mpc volume +2"                )
    , ("C-<XF86AudioLowerVolume>", spawn "mpc volume -2"                )
    , ("<XF86AudioPrev>"         , spawn "mpc prev"                     )
    , ("<XF86AudioNext>"         , spawn "mpc next"                     )
    , ("<XF86AudioPlay>"         , spawn "mpc toggle"                   )
    , ("<XF86AudioStop>"         , spawn "mpc stop"                     )

    , ("M-d M-d" , spawn "$HOME/.local/bin/dm-scripts/dm-master"     )
    , ("M-d w"   , spawn "$HOME/.local/bin/dm-scripts/dm-wallpaper"  )
    , ("M-d r"   , spawn "$HOME/.local/bin/dm-scripts/dm-record"     )
    , ("M-d p"   , spawn "$HOME/.local/bin/dm-scripts/dm-power"      )
    , ("M-d t"   , spawn "$HOME/.local/bin/dm-scripts/dm-theme"      )
    , ("M-d s"   , spawn "$HOME/.local/bin/dm-scripts/dm-screenshot" )
    , ("M-d b"   , spawn "$HOME/.local/bin/dm-scripts/dm-bookman"    )
    , ("M-d n"   , spawn "$HOME/.local/bin/dm-scripts/dm-notify"     )
    , ("M-d \\"  , spawn "$HOME/.local/bin/dm-scripts/dm-notify"     )
    , ("M-d k"   , spawn "$HOME/.local/bin/dm-scripts/dm-keys"       )

    , ("M1-<F4>", spawn "$HOME/.local/bin/dm-scripts/dm-power"         ) -- Logout Menu
    , ("M-z z"  , spawn "$HOME/.local/bin/dm-scripts/dm-power"         ) -- Logout Menu
    , ("M-z l"  , spawn "$HOME/.local/bin/dm-scripts/dm-power lock"    ) -- Lock Screen
    , ("M-z s"  , spawn "$HOME/.local/bin/dm-scripts/dm-power suspend" ) -- Suspend System
    , ("M-z p"  , spawn "$HOME/.local/bin/dm-scripts/dm-power poweroff") -- Shutdown System
    , ("M-z r"  , spawn "$HOME/.local/bin/dm-scripts/dm-power reboot"  ) -- Reboot System
    , ("M-z w"  , spawn "$HOME/.local/bin/dm-scripts/dm-power windows" ) -- Reboot to Windows

    , ("<Print>"    , spawn "$HOME/.local/bin/dm-scripts/dm-screenshot screen" ) -- Fullscreen Screenshot
    , ("M-S-<Print>", spawn "$HOME/.local/bin/dm-scripts/dm-screenshot area"   ) -- Selection Area Screenshot
    , ("M1-<Print>" , spawn "$HOME/.local/bin/dm-scripts/dm-screenshot window" ) -- Active Window Screenshot
    , ("M-<Print>"  , spawn "$HOME/.local/bin/dm-scripts/dm-screenshot full"   ) -- Full Desktop Screenshot

    , ("M-\\ \\"  , spawn "$HOME/.local/bin/dm-scripts/dm-notify recents" ) -- Show recent Notifications
    , ("M-\\ r"   , spawn "$HOME/.local/bin/dm-scripts/dm-notify recents" ) -- Show recent Notifications
    , ("M-\\ S-c" , spawn "$HOME/.local/bin/dm-scripts/dm-notify clear"   ) -- Clear all Notifications
    , ("M-\\ c"   , spawn "$HOME/.local/bin/dm-scripts/dm-notify close"   ) -- Clear last Notification
    , ("M-\\ a"   , spawn "$HOME/.local/bin/dm-scripts/dm-notify context" ) -- Open last Notification

    , ("C-M1-t"    , spawn (myTerminal)        ) -- Launch Terminal
    , ("M-<Return>", spawn (myTerminal)        ) -- Launch Terminal
    , ("M-c"       , spawn (myIde)             ) -- Launch IDE
    , ("M-S-e"     , spawn (myFileManager)     ) -- Launch File Manager
    , ("M-b"       , spawn (myWebBrowser)      ) -- Launch Web Browser
    , ("M-i"       , spawn (myIncognitoBrowser)) -- Launch Web Browser in Incognito Mode
    , ("M-p"       , spawn (myPasswordManager) ) -- Autofill Passwords
    , ("M-r"       , spawn (myLauncher)        ) -- Launch Launcher
    , ("M-S-r"     , spawn "dmenu_run"         ) -- Launch dmenu
    -- Primary
    , ("M-o t"     , spawn (myTorBrowser)      ) -- Launch Tor Browser
    , ("M-o m"     , spawn (myMusicPlayer)     ) -- Launch Music Player
    , ("M-o v"     , spawn (myVideoPlayer)     ) -- Launch Video Player
    , ("M-o s"     , spawn (myGame)            ) -- Launch Steam
    -- Secondary
    , ("C-M1-o t"  , spawn (myTextEditor)      ) -- Launch Text Editor
    , ("C-M1-o p"  , spawn (myPhotoLibrary)    ) -- Launch Photo Library
    , ("C-M1-o g"  , spawn (myImageEditor)     ) -- Launch Image Editor
    , ("C-M1-o r"  , spawn (myVectorEditor)    ) -- Launch Vector Editor
    , ("C-M1-o v"  , spawn (myVideoEditor)     ) -- Launch Video Editor
  ]

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
    [ ((shiftMask, xK_Alt_L), spawn "$HOME/.local/bin/dm-scripts/dm-lang"  ) -- Language Switching

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

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
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
