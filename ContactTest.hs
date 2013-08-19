
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Applicative
import Control.Monad

import Common

import Foundation.Bar
import Foundation.Layout
import Foundation.Sections

main :: IO ()
main = foundationGUI Config
  { tpPort       = 10000
  , tpCustomHTML = Nothing
  , tpStatic     = "static/"
  } setup

setup :: Window -> IO ()
setup w = void $ do
  getBody w #+
    [ toElement topBar
    , toElement $ 
      contacts contactEmails
    ]

-- Top Bar {{{

dummyLink :: String -> Link
dummyLink lab = Link (LabelStr lab) "#"

topBar :: TopBar
topBar = TopBar (dummyLink "Top Bar Title")
  [ subMenu "Menu Item 1"
    [ MenuSection (Just "Section Name")
      [ subMenu "Has Dropdown, Level 1"
        [ MenuSection Nothing
          [ entry "Dropdown Options"
          , entry "Dropdown Options"
          , entry "Level 2"
          , subMenu "Dropdown Options"
            [ MenuSection Nothing [ entry "Level 3" ]
            , MenuSection Nothing [ entry "Level 3, Part 2" ]
            ]
          , entry "Dropdown Options"
          , entry "Dropdown Options"
          ]
        ]
      , entry "Dropdown Option"
      , entry "Dropdown Option"
      ]
    , MenuSection (Just "Another Section Name")
      [ entry "Dropdown Option"
      , entry "Dropdown Option"
      ]
    , MenuSection Nothing [ entry "See all &rarr;" ]
    ]
  , entry "Menu Item 2"
  , subMenu "Menu Item 3"
    [ MenuSection Nothing
      [ entry "Dropdown Option"
      , entry "(A Billion) Dropdown Option(s)"
      , entry "Dropdown Option"
      ]
    , MenuSection Nothing
      [ entry "See all &rarr;"
      ]
    ]
  ]

entry :: String -> MenuEntry
entry lab = subMenu lab []

subMenu :: String -> [MenuSection] -> MenuEntry
subMenu lab = MenuEntry (dummyLink lab)

-- }}}

-- Contacts {{{

contactEmails :: [(String,String)]
contactEmails =
  [ ( "Mal Reynolds"      , "mal@serenity.bc.reb"               )
  , ( "Zoe Washburne"     , "zoe@serenity.bc.reb"               )
  , ( "Jayne Cobb"        , "jayne@serenity.bc.reb"             )
  , ( "Simon Tam"         , "doc@serenity.bc.reb"               )
  , ( "River Tam"         , "killyouwithmymind@serenity.bc.reb" )
  , ( "Hoban Washburne"   , "leafonthewind@serenity.bc.reb"     )
  , ( "Shepherd Book"     , "book@serenity.bc.reb"              )
  , ( "Kaywinnet Lee Fry" , "klee@serenity.bc.reb"              )
  , ( "Inarra Serra"      , "inara@guild.comp.all"              )
  ]

-- }}}

-- Contact {{{

contacts :: [(String,String)] -> Row [IO Element]
contacts cs = paddedRow
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
  -- , uniformLayout (colWidth 3)
  --   [
  --   ]
  ]

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
    inlineLabel "Your Name"
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
    inlineLabel "Your Email"
  , stackOnSmall (colWidth 10) $
    UI.input #*
      [ set UI.type_ "text"
      , set UI.id_   "yourEmail"
      , set placeholder "jane@smithco.com"
      ]
  ]

feedback :: [IO Element]
feedback =
  [ label #~ "What's up?"
  , UI.textarea # set UI.rows "4"
  , UI.button #
    set UI.type_ "submit" #
    set classes ["radius","button"] #~
      "Submit"
  ]

inlineLabel :: String -> IO Element
inlineLabel lab = label # set classes ["inline"] #~ lab

-- }}}

