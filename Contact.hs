
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
        (UI.h1 #+! (UI.a # set UI.html barTitle # set UI.href barLink))
    , UI.li # set classes ["toggle-topbar","menu-icon"] #+!
        UI.a # set UI.href "#" #+!
          (UI.span # set UI.html "menu")
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
  mkLabel l = UI.li #+! (label # set UI.html l)

mkEntry :: Entry -> IO Element
mkEntry se
  | null (subsections se) = UI.li #+! mkLink (sectionEntry se)
  | otherwise = UI.li # set UI.class_ "has-dropdown" #+
                  [ mkLink (sectionEntry se)
                  , mkSections (subsections se)
                  ]

mkLink :: Link -> IO Element
mkLink el = case optionLabel el of
  Raw s -> lnk # set UI.html s
  Elt e -> lnk #+! e
  where
  lnk = UI.a # set UI.href (optionLink el)

-- }}}



