Config {
   -- appearance
     font            = "xft:SF Pro Text Regular:size=9:bold:antialias=true"
   , additionalFonts = [ "xft:Font Awesome 5 Free Solid:pixelsize=16"
                       , "xft:Font Awesome 5 Brands:pixelsize=16"
                       , "xft:Mononoki:pixelsize=11:antialias=true:hinting=true"
                       ]
   , bgColor         = "#282828"
   , fgColor         = "#ebdbb2"
   , position        = TopH 24

   -- general behavior
   , lowerOnStart     = True    -- send to bottom of window stack on start
   , hideOnStart      = False   -- start with window unmapped (hidden)
   , allDesktops      = True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest     = False   -- choose widest display (multi-monitor)
   , persistent       = True    -- enable/disable hiding (True = disabled)

   -- layout
   , sepChar  = "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , iconRoot = ".xmonad/xpm/"
   , template = "%time% <fc=#7c6f64>|</fc> %UnsafeStdinReader% }{ %kbd% %default:Master% %date% "
   , commands =
        -- time and date indicators
        [ Run Date
          " %l:%M %p "
          "time" 10
          
        , Run Date
          "<box type=Bottom width=2 mb=2 color=#fb4934> %a, %d %b %Y </box>"
          "date" 3600
          
        -- Volume Indicator
        , Run Volume "default" "Master"
          [ "-t", "<box type=Bottom width=2 mb=2 color=#b8bb26> <status> <volume>% </box>"
                , "--"
                -- ON Icon
                , "-O", "<fn=1>\xf028</fn>"
                , "-C", "#b8bb26"
                -- OFF Icon
                , "-o", "<fn=1>\xf6a9</fn>"
                , "-c", "#fb4934"
                ] 10

        -- Keyboard Layout Indicator
        , Run Kbd
          [ ("us" , "US")
          , ("ru" , "RU")
          ]
        
        , Run UnsafeStdinReader
        ]
   }
