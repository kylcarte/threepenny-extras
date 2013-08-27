
module Library.Page.EditPatron where

import Graphics.UI.Threepenny.Core hiding (row)

import Foundation.Common
import Foundation.Input
import Foundation.Layout

import Library
import Library.DB.Types
import Library.Page.PatronInfo
import Library.Page.PatronSearch

import Database.SQLite.Simple

import Control.Monad

editPatronPage :: Connection -> Page
editPatronPage conn = patronSearch' "EditPatron" conn
  viewPatronInfo

viewPatronInfo :: PatronSearch ()
viewPatronInfo (drawArea,btnArea) conn
  pat searchResults
  _ = void $ do
    backBtn <- toElement $
      Button (LabelStr "Back")
        (alertBtn radiusBtnStyle) $ \self -> do
        delete self
        displayPatronTable' (drawArea,btnArea) conn
          searchResults viewPatronInfo
    pInf <- patronInfo "ViewPatronInfo" conn
              editPatronAction (patronDBId pat)
              (loadPatronFields backBtn btnArea pat)
    element drawArea # set children [] #+
      [ panel #+ pInf ]

loadPatronFields :: Element -> Element -> Patron -> PatronInfoLoad
loadPatronFields btn btnArea pat _ pf = do
  element btnArea #+ [ element btn ]
  setValue (fstNameFld pf) $ firstName   pat
  setValue (lstNameFld pf) $ lastName    pat
  setValue (phoneFld   pf) $ phoneNumber pat
  setValue (emailFld   pf) $ emailAddr   pat
  setValue (prefFld    pf) $ Just pref
  setValue (home1Fld   pf) $ homeAddr1   pat
  setValue (home2Fld   pf) $ homeAddr2   pat
  setValue (cityFld    pf) $ viewCity  $ cityStateZip pat
  setValue (stateFld   pf) $ viewState $ cityStateZip pat
  setValue (zipFld     pf) $ viewZipCd $ cityStateZip pat
  setValue (patNumFld  pf) $ show $ patronNum   pat
  where
  pref = case prefContact pat of
    Email -> "Email"
    Phone -> "Phone"

editPatronAction :: PatronInfo (Maybe Integer)
editPatronAction (alertArea,btnArea) conn _
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


