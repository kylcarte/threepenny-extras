
module Contact where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Common
import Foundation.Bar
import Foundation.Layout
import Foundation.Sections

import Data.List (intercalate)

divider :: IO Element
divider = UI.li # set classes ["divider"]

content :: [(String,String)] -> Row [IO Element]
content cs = paddedRow
  [ uniformLayout (colWidth 9)
    [ UI.h3 #~ "Get in Touch!"
    , par $ unwords
        [ "We'd love to hear from you."
        , "You can either reach out to us as a whole and one of our awesome team members will get back to you,"
        , "or if you have a specific question reach out to one of our staff."
        , "We love getting email all day "
        , "<em>every day</em>."
        ]
    , toElement $ contact cs
    ]
  , uniformLayout (colWidth 3)
    [ UI.h5 #~ "Map"
    , UI.p #+
      [ UI.a #
        set UI.href "" #
        set (data_ "reveal-id") "mapModal" #+
        [ UI.img # set UI.src "http://placehold.it/400x280"
        ]
      , UI.br
      , UI.a #
        set UI.href "" #
        set (data_ "reveal-id") "mapModal" #~
          "View Map"
      ]
    , UI.p #+
      [ string "123 Awesome St."
      , UI.br
      , string "Barsoom, MA 95155"
      ]
    ]
  ]

-- Content Panel {{{

contact :: [(String,String)] -> Sections (IO Element)
contact cs = Sections "panel" Tabs
  [ ( LabelStr "Contact Our Company"
    , UI.form #+ (toElement yourName : toElement yourEmail : toElements feedback)
    )
  , ( LabelStr "Specific Person"
    , toElement $ stackGridOnSmall 5 $ map mkContact cs
    )
  ]
  where
  mkContact (nm,addr) = UI.a # set UI.href (mkAddr addr) #+ [pic,string nm]
  mkAddr a = "mailto:" ++ a
  pic = image "http://placehold.it/200x200&amp;text=[person]"

yourName :: Row (IO Element)
yourName = collapseRow
  [ stackOnSmall (colWidth 2) $
    label # set classes ["inline"] #~ "Your Name"
  , stackOnSmall (colWidth 10) $
    UI.input #*
      [ set UI.type_ "text"
      , set UI.id_   "yourName"
      , set placeholder "Jane Smith"
      ]
  ]

yourEmail :: Row (IO Element)
yourEmail = collapseRow
  [ stackOnSmall (colWidth 2) $
    label # set classes ["inline"] #~ "Your Email"
  , stackOnSmall (colWidth 10) $
    UI.input #*
      [ set UI.type_ "text"
      , set UI.id_   "yourEmail"
      , set placeholder "jane@smithco.com"
      ]
  ]

feedback :: [IO Element]
feedback = [ lab , inp , but ]
  where
  lab = label #~ "What's up?"
  inp = UI.textarea # set UI.rows "4" # set UI.style []
  but = UI.button #
    set UI.type_ "submit" #
    set classes ["radius","button"] #~ "Submit"

-- }}}

