{-# LANGUAGE ViewPatterns #-}

module Graphics.UI.Threepenny.Foundation.Sections where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Common

-- Section {{{

data Sections a = Sections
  { sectionName :: String
  , sectionType :: SectionType
  , sectionCnts :: [(Label,a)]
  }

instance ToElements a => ToElement (Sections a) where
  toElement (Sections _ _ []) = fail "Empty Section"
  toElement (Sections nm (mkSectionType -> (typClass,typStyle)) (c:cs)) =
    divClasses ["section-container",typClass] #
      set (data_ "section") typStyle #+ (mkActive c : map mkInactive (zip cs [1..]))
    where
    mkActive (l,cnt) = let pNm = mkSectionName 0 in
      section # set classes ["section","active"] #+
        [ mkSectionLink pNm l
        , divClass "content" # set (data_ "slug") pNm #+ toElements cnt
        ]
    mkInactive ((l,cnt),i) = let pNm = mkSectionName i in
      section # set classes ["section"] #+
        [ mkSectionLink pNm l
        , divClass "content" # set (data_ "slug") pNm #+ toElements cnt
        ]
    mkSectionName :: Int -> String
    mkSectionName i = nm ++ show i
    mkSectionLink :: String -> Label -> IO Element
    mkSectionLink pNm l = UI.h5 # set classes ["title"] #+!
      (UI.a # set UI.href ("#" ++ pNm) #+ toElements l)

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

