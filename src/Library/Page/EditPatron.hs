
module Library.Page.EditPatron where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Foundation.Common
import Foundation.Sections (Page)

import Library.DBTypes
import Library.Page.PatronInfo

import Database.SQLite.Simple

editPatronPage :: Connection -> Page
editPatronPage conn = patronInfo conn editPatronAction

-- Add Patron {{{
editPatronAction :: Connection
                -> Element
                -> (Integer -> Patron)
                -> Maybe Integer
                -> IO ()
editPatronAction conn alertArea mkPat mPatNum = do
  mpn <- case mPatNum of
    -- New Patron
    Nothing -> do
      n <- genPatronNum conn
      displayInfo alertArea
        [ center #+ [ UI.h5 #~ "New Patron Number" ]
        , center #+ [ UI.h1 #~ show n ]
        ]
      return $ Just n

    -- Existing Patron
    Just n  -> do
      ns <- getPatronNumbers conn
      if n `elem` ns
        then do
          displayFailure alertArea
            "Patron Number Is Already Taken"
          return Nothing
        else do
          displaySuccess alertArea
            "Patron Added!"
          return $ Just n

  -- All Set to Go
  whenJust mpn $ \pn -> do
    let pat = mkPat pn
    putStrLn $ concat
      [ "Adding Patron: "
      , firstName pat , " " , lastName pat
      , " : "   , show pn
      ]
    insertPatron conn pat

-- }}}

