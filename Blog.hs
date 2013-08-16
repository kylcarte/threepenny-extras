
module Blog where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Applicative
import Data.List (intersperse)
import Data.IORef

import Common

-- Nav Bar {{{

navBar :: IO Element
navBar = row12ColClass
  [ navBarRight
      [ ("Link 1","#")
      , ("Link 2","#")
      , ("Link 3","#")
      , ("Link 4","#")
      ]
  , UI.h1 #+
    [ string "Blog "
    , UI.small #+! string "This is my blog. It's awesome"
    ]
  ]

row12ColClass :: [IO Element] -> IO Element
row12ColClass es = rowClass #+ map (twelveColClass #+!) es

twelveColClass :: IO Element
twelveColClass = divClasses ["large-12","columns"]

navBarRight :: [(String,String)] -> IO Element
navBarRight bs = divClasses ["nav-bar","right"] #+ [buttonGroup bs]

buttonGroup :: [(String,String)] -> IO Element
buttonGroup bs = UI.ul # set UI.class_ "button-group" #+ map mkBut bs
  where
  mkBut (lab,lnk) = buttonLink lab lnk

-- }}}

-- Article Entry {{{

articleEntry :: String -> String -> String -> [String] -> [String]-> IO Element
articleEntry title author date besideImg belowImg = article #+ concat
  [ [ UI.h3 #+! link title "#"
    , UI.h6 #+
      [ string "Written by "
      , link author "#"
      , string $ " on " ++ date
      ]
    , row6ColClass
      [ map par besideImg
      , [ image "http://placehold.it/400x240&text=[img]" ]
      ]
    ]
  , map par belowImg
  ]

row6ColClass :: [[IO Element]] -> IO Element
row6ColClass es = rowClass #+ map (sixColClass #+) es

sixColClass :: IO Element
sixColClass = divClasses ["large-6","columns"]

-- }}}

-- Side Bar {{{

sideBar :: String -> IO Element
sideBar txt = threeColAside #+
  [ UI.h5 #+! string "Categories"
  , UI.ul # set UI.class_ "side-nav" #+
    listItems
      [ link "News"    "#"
      , link "Code"    "#"
      , link "Design"  "#"
      , link "Fun"     "#"
      , link "Weasels" "#"
      ]
  , panelClass #+
    [ UI.h5 #+! string "Featured"
    , par txt
    , link "Read More &rarr;" "#"
    ]
  ]

panelClass :: IO Element
panelClass = divClass "panel"

threeColAside :: IO Element
threeColAside = threeColClass aside

threeColClass :: IO Element -> IO Element
threeColClass el = el # set classes ["large-3","columns"]

-- }}}

-- Footer {{{

blogFooter :: IO Element
blogFooter = footer # set UI.class_ "row" #+
  [ twelveColClass #+
    [ UI.hr
    , row6ColClass $ map single
      [ par "&copy; Copyright no one at all. Go to town."
      , UI.ul # set classes ["inline-list","right"] #+
        listItems
          [ link "Link 1" "#"
          , link "Link 2" "#"
          , link "Link 3" "#"
          , link "Link 4" "#"
          ]
      ]
    ]
  ]

-- }}}

