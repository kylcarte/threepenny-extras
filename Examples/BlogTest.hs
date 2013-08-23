{-# LANGUAGE OverloadedStrings #-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Foundation.Common
import Examples.Blog

import Control.Applicative
import Control.Monad
import Data.List (intersperse)

main :: IO ()
main = do
  startGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = "static/"
    } setup

setup :: Window -> IO ()
setup w = void $ do
  UI.addStyleSheet w "foundation.css"
  UI.addStyleSheet w "normalize.css"
  getBody w #+ mainPage

mainPage :: [IO Element]
mainPage =
  [ navBar
  , rowClass #+
    [ articles
    , blogSideBar
    ]
  , blogFooter
  ]

articles :: IO Element
articles = UI.div # set UI.class_ "large-9 columns" # set role "content" #+
  intersperse UI.hr
    [ baconArticle
    , baconArticle
    , baconArticle
    , baconArticle
    , baconArticle
    ]

blogSideBar :: IO Element
blogSideBar = sideBar $ unwords
  [ "Pork drumstick turkey fugiat."
  , "Tri-tip elit turducken pork chop in."
  , "Swine short ribs meatball irure bacon nulla pork belly cupidatat meatloaf cow."
  ]

baconArticle :: IO Element
baconArticle = articleEntry "Article Test" "Kyle Carter" "Aug 12, 2003"
  [ unwords
    [ "Bacon ipsum dolor sit amet nulla ham qui sint exercitation eiusmod commodo, chuck duis velit."
    , "Aute in reprehenderit, dolore aliqua non est magna in labore pig pork biltong."
    , "Eiusmod swine spare ribs reprehenderit culpa."
    ]
  , unwords
    [ "Boudin aliqua adipisicing rump corned beef."
    , "Nulla corned beef sunt ball tip, qui bresaola enim jowl."
    , "Capicola short ribs minim salami nulla nostrud pastrami."
    ]
  ]
  [ unwords
    [ "Pork drumstick turkey fugiat."
    , "Tri-tip elit turducken pork chop in."
    , "Swine short ribs meatball irure bacon nulla pork belly cupidatat meatloaf cow."
    , "Nulla corned beef sunt ball tip, qui bresaola enim jowl."
    , "Capicola short ribs minim salami nulla nostrud pastrami."
    , "Nulla corned beef sunt ball tip, qui bresaola enim jowl."
    , "Capicola short ribs minim salami nulla nostrud pastrami."
    ]
  , unwords
    [ "Pork drumstick turkey fugiat."
    , "Tri-tip elit turducken pork chop in."
    , "Swine short ribs meatball irure bacon nulla pork belly cupidatat meatloaf cow."
    , "Nulla corned beef sunt ball tip, qui bresaola enim jowl."
    , "Capicola short ribs minim salami nulla nostrud pastrami."
    , "Nulla corned beef sunt ball tip, qui bresaola enim jowl."
    , "Capicola short ribs minim salami nulla nostrud pastrami."
    ]
  ]

