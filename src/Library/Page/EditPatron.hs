
module Library.Page.EditPatron where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Foundation.Common
import Foundation.Sections (Page)

import Library.DBTypes
import Library.Page.PatronInfo
import Library.Page.PatronSearch

import Database.SQLite.Simple

import Control.Monad

editPatronPage :: Connection -> Page
editPatronPage conn = patronSearch
  conn
  viewPatronInfo
  ()

viewPatronInfo :: PatronSearch ()
viewPatronInfo
  area
  conn
  thisPatron
  searchResults
  _ = void $ do
    pi <- patronInfo editPatronAction ()
    element area # set children [] #+ pi
    
editPatronAction :: PatronInfo ()
editPatronAction = undefined

