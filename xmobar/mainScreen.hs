Config {
   -- appearance
     font            = "xft:SF Pro Text Regular:size=9:bold:antialias=true"
   , additionalFonts = [ "xft:Font Awesome 5 Free Solid:pixelsize=16"
                       , "xft:Font Awesome 5 Brands:pixelsize=16"
                       , "xft:Font Awesome 5 Free Solid:pixelsize=14"
                       , "xft:Mononoki:pixelsize=11:antialias=true:hinting=true"
                       ]
   , bgColor         = "#282828"
   , fgColor         = "#ebdbb2"
   , position        = TopH 24
   , lowerOnStart     = True    -- send to bottom of window stack on start
   , hideOnStart      = False   -- start with window unmapped (hidden)
   , allDesktops      = True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest     = False   -- choose widest display (multi-monitor)
   , persistent       = True    -- enable/disable hiding (True = disabled)
   , sepChar  = "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , iconRoot = ".config/xmonad/xpm/"
   , commands =
        [ Run Com "echo" ["<fc=#7c6f64>|</fc>"] "separator" 3600
        , Run Date
          " %l:%M %p"
          "time" 10
        , Run Com "echo" ["<box type=Bottom width=2 mb=2 color=#fb4934>  <fn=3>\xf0f3</fn> "] "u_icon" 3600
        , Run Com ".config/xmonad/scripts/updates" [] "updates" 3600
        , Run DynNetwork
          ["-t", "<box type=Bottom width=2 mb=2 color=#8ec07c>  <fn=3>\xf0ac</fn>  <rx> <fn=3>\xf309\xf30c</fn> <tx> </box>"
               , "-S", "True"
               , "--"
               , "--devices", "eno1,wlan0,enp2s0f0"
               ] 20
        , Run CoreTemp
          ["-t", "<box type=Bottom width=2 mb=2 color=#d3869b>  <fn=3>\xf2db</fn>  <core0>° "
               -- High CPU Temp
               , "-H", "70"
               , "-h", "#fb4934"
               -- Low CPU Temp
               , "-L", "40"
               , "-l", "#b8bb26"
               ] 20
        , Run Cpu
          ["-t", "(<total>%)  </box>"
               -- High CPU Load
               , "-H", "80"
               , "-h", "#fb4934"
               -- Low CPU Load
               , "-L", "5"
               , "-l", "#b8bb26"
               ] 20
        , Run Memory
          ["-t", "<box type=Bottom width=2 mb=2 color=#83a598>  <fn=3>\xf538</fn>  <used> M (<usedratio>%)  </box>"
               ] 20
        , Run Volume "default" "Master"
          ["-t", "<box type=Bottom width=2 mb=2 color=#b8bb26>  <status>  <volume>%  </box>"
               , "--"
               -- ON Icon
               , "-O", "<fn=3>\xf028</fn>"
               , "-C", "#b8bb26"
               -- OFF Icon
               , "-o", "<fn=3>\xf6a9</fn>"
               , "-c", "#fb4934"
               ] 10
        , Run Uptime
          ["-t", "<box type=Bottom width=2 mb=2 color=#fabd2f>  <fn=3>\xf0aa</fn>  <days>d <hours>h  </box>"
               ] 3600
        , Run Date
          "<box type=Bottom width=2 mb=2 color=#fb4934>  <fn=3>\xf133</fn>  %a, %d %b %Y  </box>"
          "date" 3600
        , Run Kbd
          [ ("us" , "US ")
          , ("ru" , "RU ")
          ]
        , Run Com ".config/xmonad/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20
        , Run UnsafeStdinReader
        ]
  , template = "%time% %separator% %UnsafeStdinReader% }{ %kbd% %u_icon% %updates%  </box> %dynnetwork% %coretemp%%cpu% %memory% %default:Master% %uptime% %date% %trayerpad%"
   }
