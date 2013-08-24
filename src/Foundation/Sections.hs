{-# LANGUAGE ViewPatterns #-}

module Foundation.Sections where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Foundation.Common

-- Sections {{{

type Section a = (Label,a)
type Page = Section (IO [IO Element])

data Sections a = Sections
  { sectionName :: String
  , sectionType :: SectionType
  , sectionCnts :: [Section a]
  }

instance ToElements a => ToElement (Sections a) where
  toElement (Sections _ _ []) = fail "Empty Section"
  toElement (Sections nm (mkSectionType -> (typClass,typStyle)) (c:cs)) =
    divClasses ["section-container",typClass] #
      set (data_ "section") typStyle #+ (mkActive c : map mkInactive (zip cs [1..]))
    where
    mkActive (l,cnt) = let pNm = mkSectionName 0 in do
      ces <- toElements cnt
      section # set classes ["section","active"] #+
        [ mkSectionLink pNm l
        , divClass "content" # set (data_ "slug") pNm #+ map element ces
        ]
    mkInactive ((l,cnt),i) = let pNm = mkSectionName i in do
      ces <- toElements cnt
      section # set classes ["section"] #+
        [ mkSectionLink pNm l
        , divClass "content" # set (data_ "slug") pNm #+ map element ces
        ]
    mkSectionName :: Int -> String
    mkSectionName i = nm ++ show i
    mkSectionLink :: String -> Label -> IO Element
    mkSectionLink pNm l = do
      l' <- toElements l
      UI.h5 # set classes ["title"] #+!
        (UI.a # set UI.href ("#" ++ pNm) #+ map element l')

-- }}}

-- SectionType {{{

data SectionType
  = Auto
  | Accordian
  | Tabs
  | VertTabs
  | VertNav
  | HorzNav

mkSectionType :: SectionType -> (String,String)
mkSectionType Auto      = ("auto"           , ""              )
mkSectionType Accordian = ("accordian"      , "accordian"     )
mkSectionType Tabs      = ("tabs"           , "tabs"          )
mkSectionType VertTabs  = ("vertical-tabs"  , "vertical-tabs" )
mkSectionType VertNav   = ("vertical-nav"   , "vertical-nav"  )
mkSectionType HorzNav   = ("horizontal-nav" , "horizontal-nav")

-- }}}

