{-# LANGUAGE OverloadedStrings #-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Applicative
import Control.Monad

import Common
import Contact

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
  [ top
  ]

-- Top Bar {{{

top :: IO Element
top = topBar "Top Bar Title" "#"
  [ entry "Menu Item 1" "#"
    [ Section (Just "Section Name")
      [ entry "Has Dropdown, Level 1" "#"
        [ Section Nothing
          [ leaf "Dropdown Options" "#"
          , leaf "Dropdown Options" "#"
          , leaf "Level 2"          "#"
          , entry "Dropdown Options" "#"
            [ Section Nothing
              [ leaf "Level 3" "#"
              ]
            , Section Nothing
              [ leaf "Level 3, Part 2" "#"
              ]
            ]
          , leaf "Dropdown Options" "#"
          , leaf "Dropdown Options" "#"
          ]
        ]
      , leaf "Dropdown Option" "#"
      , leaf "Dropdown Option" "#"
      ]
    , Section (Just "Another Section Name")
      [ leaf "Dropdown Option" "#"
      , leaf "Dropdown Option" "#"
      ]
    , Section Nothing
      [ leaf "See all &rarr;" "#"
      ]
    ]
  , leaf "Menu Item 2" "#"
  , entry "Menu Item 3" "#"
    [ Section Nothing
      [ leaf "Dropdown Option" "#"
      , leaf "(A Billion) Dropdown Option(s)" "#"
      , leaf "Dropdown Option" "#"
      ]
    , Section Nothing
      [ leaf "See all &rarr;" "#"
      ]
    ]
  ]

leaf :: String -> String -> Entry
leaf lab lnk = entry lab lnk []

entry :: String -> String -> [Section] -> Entry
entry lab lnk = Entry (rawLink lab lnk)

rawLink :: String -> String -> Link
rawLink lab lnk = Link (Raw lab) lnk

-- }}}

