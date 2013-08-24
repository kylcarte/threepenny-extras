{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.Threepenny.Core

import Foundation.Layout
import Foundation.Sections
import Foundation.Common

import Library.DBTypes
import Library.Page.NewPatron
import Library.Page.EditPatron

import Database.SQLite.Simple

import Control.Monad

main :: IO ()
main = do
  conn <- initDB "library.db"
  foundationGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = "static/"
    } $ setup conn

setup :: Connection -> Window -> IO ()
setup conn w = void $ do
  p <- toElement $ mainPage conn
  getBody w #+ [ element p ]

mainPage :: Connection -> Row (IO Element)
mainPage conn = collapseRow
  [ stackOnSmall (centered 6) $
    toElement $
    Sections "Tabs" Tabs
      [ newPatron conn
      , editPatron conn
      ]
  ]

