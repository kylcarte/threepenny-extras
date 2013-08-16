
module Contact where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Common
import Data.List (intercalate)

-- Top Bar {{{

divider :: IO Element
divider = UI.li # set UI.class_ "divider"

dropDownLinks :: [(IO Element,String)] -> IO Element
dropDownLinks ls = UI.ul # set UI.class_ "dropdown" #+ map mkLink ls
  where
  mkLink (lab,lnk) = UI.li #+! UI.a # set UI.href lnk #+! lab

data HTML
  = Raw String
  | Elt (IO Element)

data Link = Link
  { optionLabel :: HTML
  , optionLink  :: String
  }

data Entry = Entry
  { sectionEntry :: Link
  , subsections  :: [Section]
  }

data Section = Section
  { sectionName    :: Maybe String
  , sectionEntries :: [Entry]
  }

topBar :: String -> String -> [Entry] -> IO Element
topBar barTitle barLink ses = nav # set UI.class_ "top-bar" #+
  [ UI.ul # set UI.class_ "title-area" #+
    [ UI.li # set UI.class_ "name" #+!
        (UI.h1 #+! (UI.a # set UI.href barLink #~ barTitle))
    , UI.li # set classes ["toggle-topbar","menu-icon"] #+!
        UI.a # set UI.href "#" #+!
          (UI.span #~ "menu")
    ]
  , mkTopBarSection ses
  ]

mkTopBarSection :: [Entry] -> IO Element
mkTopBarSection ses = section # set UI.class_ "top-bar-section" #+! barEntryList
  where
  barEntryList = UI.ul # set UI.class_ "right" #+ concatMap mkBarEntry ses
  mkBarEntry se = [ divider , mkEntry se ]

mkSections :: [Section] -> IO Element
mkSections ss = UI.ul # set UI.class_ "dropdown" #+ intercalate [divider] ss'
  where
  ss' = map mkSection ss

mkSection :: Section -> [IO Element]
mkSection s = case sectionName s of
  Nothing -> entries
  Just nm -> mkLabel nm : entries
  where
  entries = map mkEntry $ sectionEntries s
  mkLabel l = UI.li #+! (label #~ l)

mkEntry :: Entry -> IO Element
mkEntry se
  | null (subsections se) = UI.li #+! mkLink (sectionEntry se)
  | otherwise = UI.li # set UI.class_ "has-dropdown" #+
                  [ mkLink (sectionEntry se)
                  , mkSections (subsections se)
                  ]

mkLink :: Link -> IO Element
mkLink el = case optionLabel el of
  Raw s -> lnk #~ s
  Elt e -> lnk #+! e
  where
  lnk = UI.a # set UI.href (optionLink el)

-- }}}

-- Contact {{{

content :: [(String,String)] -> IO Element
content cs = rowClass #+!
  (divClasses ["large-9","columns"] #+
    [ UI.h3 #~ "Get in Touch!"
    , par $ unwords
        [ "We'd love to hear from you."
        , "You can either reach out to us as a whole and one of our awesome team members will get back to you,"
        , "or if you have a specific question reach out to one of our staff."
        , "We love getting email all day "
        , "<em>all day</em>."
        ]
    , contact cs
    ])

contact :: [(String,String)] -> IO Element
contact cs = divClasses ["section-container","auto"] #
  set (data_ "section") "" #+
  [ section # set classes ["section","active"] #
    set UI.style [("padding-top","52px")] #+
      [ UI.h5 # set UI.class_ "title" #+!
          link "Contact Our Company" "#panel1"
      , divClass "content" # set (data_ "slug") "panel1" #+!
        (UI.form #+ ([yourName,yourEmail] ++ feedback))
      ]
  , section # set UI.class_ "section" #+
    [ UI.h5 # set UI.class_ "title" #+!
        link "Specific Person" "#panel2"
    , divClass "content" # set (data_ "slug") "panel2" #+!
      (UI.ul # set UI.class_ "large-block-grid-5" #+ map mkContact cs)
    ]
  ]
  where
  mkContact (nm,addr) = UI.li #+!
    (UI.a # set UI.href (mkAddr addr) #+ [pic,string nm])
  mkAddr a = "mailto:" ++ a
  pic = image "http://placehold.it/200x200&amp;text=[person]"

yourName :: IO Element
yourName = divClasses ["row","collapse"] #+
  [ divClasses ["large-2" ,"columns"] #+! lab
  , divClasses ["large-10","columns"] #+! inp
  ]
  where
  lab = label # set UI.class_ "inline" #~ "Your Name"
  inp = UI.input                 #
    set UI.type_ "text"          #
    set UI.id_   "yourName"      #
    set placeholder "Jane Smith"

yourEmail :: IO Element
yourEmail = divClasses ["row","collapse"] #+
  [ divClasses ["large-2" ,"columns"] #+! lab
  , divClasses ["large-10","columns"] #+! inp
  ]
  where
  lab = label # set UI.class_ "inline" #~ "Your Email"
  inp = UI.input                        #
    set UI.type_ "text"                 #
    set UI.id_   "yourEmail"            #
    set placeholder "jane@smithco.com"

feedback :: [IO Element]
feedback = [lab,inp,but]
  where
  lab = label #~ "What's up?"
  inp = UI.textarea # set UI.rows "4"
  but = UI.button #
    set UI.type_ "submit" #
    set classes ["radius","button"] #~ "Submit"

-- }}}

