
module Graphics.UI.Threepenny.Foundation.Bar where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Common

import Control.Applicative
import Data.List (intercalate)

-- MenuEntry {{{

data MenuEntry = MenuEntry
  { entryLink    :: Link
  , subsections  :: [MenuSection]
  }

instance ToElement MenuEntry where
  toElement (MenuEntry lnk ss)
    | null ss = UI.li #+! toElement lnk
    | otherwise = UI.li # set classes ["has-dropdown"] #+
                    [ toElement lnk
                    , mkSections
                    ]
    where
    mkSections = do 
      ss' <- mapM toElements ss
      UI.ul # set classes ["dropdown"] #+ intercalate [divider] (map (map element) ss')

-- }}}

-- MenuSection {{{

data MenuSection = MenuSection
  { sectionName    :: Maybe String
  , sectionEntries :: [MenuEntry]
  }

instance ToElements MenuSection where
  toElements (MenuSection Nothing es) = toElements es
  toElements (MenuSection (Just nm) es) = (:) <$> mkSectionLabel nm <*> toElements es
    where
    mkSectionLabel l = UI.li #+! (label #~ l)

-- }}}

-- TopBar {{{

data TopBar = TopBar
  { topBarLink    :: Link
  , topBarEntries :: [MenuEntry]
  }

instance ToElement TopBar where
  toElement (TopBar lnk mes) = nav # set classes ["top-bar"] #+
    [ UI.ul # set classes ["title-area"] #+
      [ UI.li # set classes ["name"] #+!
          (UI.h1 #+! toElement lnk)
      , UI.li # set classes ["toggle-topbar","menu-icon"] #+!
          link "Menu" "#"
      ]
    , mkTopBarSection
    ]
    where
    mkTopBarSection = section # set classes ["top-bar-section"] #+!
      (UI.ul # set classes ["right"] #+ concatMap mkBarEntry mes)
    mkBarEntry me = [ divider , toElement me ]

-- }}}

