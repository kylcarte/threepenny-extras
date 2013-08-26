
module Library.Page.PatronInfo where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (row)
import Database.SQLite.Simple

import Foundation.Common
import Foundation.Input
import Foundation.Layout
import Foundation.Sections (Page)
import Library
import Library.DBTypes

import Control.Applicative hiding (optional)

type PatronInfo extra
  =  (Element,Element)
  -> Connection
  -> PatronFields
  -> String -> String
  -> PhoneNumber -> Email -> Contact
  -> Addr -> Addr
  -> CityStateZip
  -> Maybe Integer -- PatronNumber
  -> extra
  -> IO ()

type PatronInfoLoad = (Element,Element) -> PatronFields -> IO ()

-- Patron Info {{{

noLoadAction :: PatronInfoLoad
noLoadAction _ _ = return ()

patronInfo' :: String -> Connection -> PatronInfo () -> PatronInfoLoad -> Page
patronInfo' pg conn buttonAct loadAct = patronInfo pg conn buttonAct () loadAct

patronInfo :: String
           -> Connection
           -> PatronInfo extra
           -> extra
           -> PatronInfoLoad
           -> Page
patronInfo pageNm conn buttonAct extra loadAct = do
  pf        <- mkPatronFields pageNm
  alertArea <- UI.div
  btnArea   <- UI.div
  loadAct (alertArea,btnArea) pf
  let resetFlds = clearPatronFields pf
  clearBtn  <- toElement $
    Button (LabelStr "Clear") (secondaryBtn radiusBtnStyle) $ const $
      resetFlds
  submitBtn <- toElement $ 
    Button (LabelStr "Submit") radiusBtnStyle $ const $ do
      validatePatronFields pf
        buttonAct
        (alertArea,btnArea)
        conn
        extra
  let renderFld fld = toElement $ fieldContent $ fld pf
  return
    [ renderFld fstNameFld , renderFld lstNameFld
    , renderFld emailFld   , renderFld phoneFld   , renderFld prefFld
    , UI.hr
    , renderFld home1Fld   , renderFld home2Fld
    , renderFld cityFld    , renderFld stateFld   , renderFld zipFld
    , UI.hr
    , renderFld patNumFld
    , UI.h5 #+ [ UI.small #~ "* Required Fields" ]
    , UI.hr
    , pad $
      split
      [ ( 9 , pad $ center #+! element alertArea )
      , ( 3 , right #+ 
                [ element submitBtn
                , UI.br
                , element clearBtn
                , UI.br
                , element btnArea
                ])
      ]
    ]

-- }}}

-- Patron Fields {{{

data PatronFields = PatronFields
  { fstNameFld :: Field String
  , lstNameFld :: Field String
  , emailFld   :: Field String
  , phoneFld   :: Field String
  , prefFld    :: Field (Maybe String)
  , home1Fld   :: Field String
  , home2Fld   :: Field String
  , cityFld    :: Field String
  , stateFld   :: Field String
  , zipFld     :: Field String
  , patNumFld  :: Field String
  }



mkPatronFields :: String -> IO PatronFields
mkPatronFields pgName =
      PatronFields
  <$> required textField "First Name"
  <*> required textField "Last Name"
  <*> required textField "Email Address"
  <*> required textField "Phone Number"
  <*> required
        (radsField
         prefRads)       "Preferred Contact"
  <*> optional textField "Home Address 1"
  <*> optional textField "Home Address 2"
  <*> optional textField "City"
  <*> optional
        (dropdownField
         statesDropdown) "State"
  <*> optional textField "Zip Code"
  <*> optional textField "Patron Number"
  where
  prefRads = Radios (pgName ++ "prefcont") Nothing $ map radOpt
    [ "Email"
    , "Phone"
    ]
  statesDropdown = Dropdown "states" True $ map opt
    [ "AL" , "AK" , "AZ" , "AR" , "CA"
    , "CO" , "CT" , "DE" , "FL" , "GA"
    , "HI" , "ID" , "IL" , "IN" , "IA"
    , "KS" , "KY" , "LA" , "ME" , "MD"
    , "MA" , "MI" , "MN" , "MS" , "MO"
    , "MT" , "NE" , "NV" , "NH" , "NJ"
    , "NM" , "NY" , "NC" , "ND" , "OH"
    , "OK" , "OR" , "PA" , "RI" , "SC"
    , "SD" , "TN" , "TX" , "UT" , "VT"
    , "VA" , "WA" , "WV" , "WI" , "WY"
    ]



clearPatronFields :: PatronFields -> IO ()
clearPatronFields pf = do
  setValue (fstNameFld pf) ""
  setValue (lstNameFld pf) ""
  setValue (emailFld   pf) ""
  setValue (phoneFld   pf) ""
  setValue (prefFld    pf) Nothing
  setValue (home1Fld   pf) ""
  setValue (home2Fld   pf) ""
  setValue (cityFld    pf) ""
  setValue (stateFld   pf) ""
  setValue (zipFld     pf) ""
  setValue (patNumFld  pf) ""



validatePatronFields :: PatronFields
                     -> PatronInfo extra
                     -> (Element,Element)
                     -> Connection
                     -> extra
                     -> IO ()
validatePatronFields pf validatedFn (alertArea,btnArea) conn extra =
  validate fstNameFld notNull     $ \fstName ->
  validate lstNameFld notNull     $ \lstName ->
  validate emailFld   emailValid  $ \email   ->
  validate phoneFld   phoneValid  $ \phone   ->
  validate prefFld    havePref    $ \pref    ->
  validate zipFld     zipValid    $ \zipCd   ->
  validate patNumFld  patNumValid $ \mpn     -> do
    -- Input Validation Success
    home1   <- getValue $ home1Fld pf
    home2   <- getValue $ home2Fld pf
    city    <- getValue $ cityFld  pf
    state   <- getValue $ stateFld pf
    validatedFn (alertArea,btnArea) conn pf
      fstName lstName
      phone email pref
      home1 home2
      (CSZ city state zipCd)
      mpn
      extra
  where
  validate fld = validateWithArea alertArea $ fld pf

-- }}}

