{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.Threepenny.Core

import Foundation.Layout
import Foundation.Sections
import Foundation.Common

import Control.Monad

import Library.Page.NewPatron

main :: IO ()
main = do
  foundationGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = "static/"
    } setup

setup :: Window -> IO ()
setup w = void $ do
  p <- toElement mainPage
  getBody w #+ [ element p ]

mainPage :: Row (IO Element)
mainPage = collapseRow
  [ stackOnSmall (centered 6) $
    toElement $
    Sections "Tabs" Tabs
      [ newPatron
      ]
  ]

