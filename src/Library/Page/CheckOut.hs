
module Library.Page.CheckOut where

import Graphics.UI.Threepenny.Core hiding (row)

import Foundation.Common
import Foundation.Input
-- import Foundation.Layout

import Library
import Library.DB.Types
import Library.Page.PatronInfo
import Library.Page.PatronSearch

import Database.SQLite.Simple

import Control.Monad

checkOutPage :: Connection -> Page
checkOutPage conn = patronSearch' "CheckOut" conn
  checkOutItem

checkOutItem :: PatronSearch ()
checkOutItem (drawArea,btnArea) conn
  pat searchResults
  _ = void $ do
    backBtn <- toElement $
      Button (LabelStr "Back")
        (alertBtn radiusBtnStyle) $ \self -> do
        delete self
        displayPatronTable' (drawArea,btnArea) conn
          searchResults checkOutItem
    pInf <- patronInfo "CheckOutItem" conn
              checkOutAction
              (patronDBId pat)
              noLoadAction
    element drawArea # set children [] #+
      [ panel #+ pInf ]

checkOutAction :: PatronInfo (Maybe Integer)
checkOutAction (alertArea,_) conn _
  fstNm lstNm
  phone email pref
  home1 home2
  csz mpn
  mId = case (mpn,mId) of
    (Nothing,_) -> displayFailure alertArea
                     "Patron Number Missing"
    (_,Nothing) -> fail "Bug: Wasn't given a DB Id Number for Patron"
    (Just pn,Just idNo) -> do
      taken <- patronNumberTaken conn idNo pn
      if taken
        then displayFailure alertArea
               "Patron Number Is Taken"
        else do
          displaySuccess alertArea "Patron Updated!"
          let pat = Patron mId
                fstNm lstNm
                phone email pref
                home1 home2
                csz pn
          updatePatron conn pat


