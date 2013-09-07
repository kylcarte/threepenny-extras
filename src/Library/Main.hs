{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.Threepenny.Core

import Foundation.Layout
import Foundation.Sections
import Foundation.Common

import Library.DB
import Library.Page.AddPatron
import Library.Page.EditPatron
import Library.Page.CheckOutSearch

import Control.Monad

dbPaths :: DBPaths
dbPaths = DBPaths
  "db/patrons.db"
  "db/checkouts.db"
  "db/books.db"

main :: IO ()
main = do
  db <- initDBs dbPaths
  foundationGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = "static/"
    } $ setup db

setup :: DB -> Window -> IO ()
setup db w = void $ do
  p <- toElement $ mainPage db
  getBody w #+ [ element p ]

mainPage :: DB -> Row (IO Element)
mainPage db = collapseRow
  [ stackOnSmall (centered 10) $
    toElement $
    Sections "Tabs" Tabs
      [ ( LabelStr "Add Patron"  , addPatronPage  $ patronsDB db   )
      , ( LabelStr "Edit Patron" , editPatronPage $ checkOutsDB db )
      , ( LabelStr "Check Out"   , checkOutPage   $ checkOutsDB db )
      ]
  ]

