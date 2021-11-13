Config {
   -- appearance
     font            = "xft:SF Pro Text Regular:size=9:bold:antialias=true"
   , additionalFonts = [ "xft:Mononoki:pixelsize=11:antialias=true:hinting=true"
                       , "xft:Font Awesome 5 Free Solid:pixelsize=12"
                       , "xft:Font Awesome 5 Brands:pixelsize=12"
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
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "%time% %UnsafeStdinReader% }{ %kbd% %date% "
   , commands =
        -- time and date indicators
        [ Run Date "<box type=Bottom width=2 mb=2> %l:%M %p  </box>" "time" 10
        , Run Date "<box type=Bottom width=2 mb=2> %a, %d %b %Y </box>" "date" 3600

        -- keyboard layout indicator
        , Run Kbd            [ ("us" , "US")
                             , ("ru" , "RU")
                             ]
        
        , Run UnsafeStdinReader
        ]
   }
