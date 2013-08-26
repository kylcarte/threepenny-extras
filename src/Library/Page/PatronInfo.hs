
module Library.Page.PatronInfo where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (row)

import Foundation.Common
import Foundation.Input
import Foundation.Layout
import Foundation.Sections (Page)

import Library
import Library.DBTypes

import Database.SQLite.Simple

import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec.Rfc2822 as P
import System.Random hiding (split)

import Control.Applicative hiding (optional)
import Control.Monad
import Data.Char (isDigit)

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

optional :: (String -> String -> IO (Field a))
         -> String
         -> IO (Field a)
optional f typ = f typ typ
required :: (String -> String -> IO (Field a))
         -> String
         -> IO (Field a)
required f typ = f typ (typ ++ "*")

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

-- Success/Failure Alerts {{{

displaySuccess :: Element -> String -> IO ()
displaySuccess e msg = void $ do
  alrt <- alertBox ["success","round"] msg
  element e # set children [ alrt ]

displayFailure :: Element -> String -> IO ()
displayFailure e msg = void $ do
  alrt <- alertBox ["alert","round"] msg
  element e # set children [ alrt ]

displayInfo :: Element -> [IO Element] -> IO ()
displayInfo e es = void $ do
  alrt <- toElement $ calloutPanel es
  element e # set children [ alrt ]

calloutPanel :: [IO Element] -> Column (IO Element)
calloutPanel es = uniformLayout (centered 8) $
  UI.div # set classes ["callout","panel"] #+ es

alertBox :: [String] -> String -> IO Element
alertBox cs msg = UI.div #
  set (data_ "alert") "" #
  set classes ("alert-box" : cs) #~
    msg

-- }}}

-- Input Validation {{{

type Validate a = Either String a
vdtFail :: String -> Validate a
vdtFail = Left
vdtSucc :: a -> Validate a
vdtSucc = Right

validateWithArea :: Element -> Field a -> (a -> Validate res) -> (res -> IO ()) -> IO ()
validateWithArea e fld test onSuccess = do
  v <- getValue fld
  case test v of
    Left  err -> displayFailure e (err ++ " " ++ fieldType fld)
    Right res -> onSuccess res

-- Names
notNull :: String -> Validate String
notNull s
  | null s   = vdtFail "Missing"
  | otherwise = vdtSucc s

-- Email
emailValid :: String -> Validate String
emailValid em = if null em
  then vdtFail "Missing"
  else case parseEmail em of
         Left _  -> vdtFail "Malformed"
         Right _ -> vdtSucc em
  where
  parseEmail = P.parse P.addr_spec ""

-- Phone Number
phoneValid :: String -> Validate String
phoneValid ph = if null ph
  then vdtFail "Missing"
  else if all (\c -> isDigit c || elem c phoneAcceptableNonDigits) ph
    then vdtSucc $ filter isDigit ph
    else vdtFail "Malformed"

phoneAcceptableNonDigits :: [Char]
phoneAcceptableNonDigits = " ()-"

-- Zip Code
zipValid :: String -> Validate String
zipValid zp
  | null zp   = vdtSucc ""
  | length zp == 5 && all isDigit zp = vdtSucc zp
  | length zp == 10 &&
    zp !! 5 == '-' && 
    all isDigit (take 5 zp) &&
    all isDigit (take 4 $ reverse zp) = vdtSucc $ take 5 zp
  | otherwise = vdtFail "Malformed"

-- Preferred Contact
havePref :: Maybe String -> Validate Contact
havePref (Just "Email") = vdtSucc Email
havePref (Just "Phone") = vdtSucc Phone
havePref (Just _)       = vdtFail "Malformed"
havePref Nothing        = vdtFail "Missing"

-- Patron Number
patNumValid :: String -> Validate (Maybe Integer)
patNumValid n
  | null n    = vdtSucc Nothing
  | length n == numDigitsPatron && all isDigit n
              = vdtSucc $ Just $ read n
  | otherwise = vdtFail "Malformed"

-- }}}

-- Patron Number {{{

genPatronNum :: Connection -> IO Integer
genPatronNum = genPatronNumDigits numDigitsPatron

genPatronNumDigits :: Int -> Connection -> IO Integer
genPatronNumDigits digits conn = do
  ns <- map fst <$> getPatronNumbers conn
  go ns
  where
  go ns = do
    n <- randomRIO (10 ^ (digits - 1), (10 ^ digits) - 1)
    if n `elem` ns
      then go ns
      else return n

-- }}}

-- Helpers {{{

data Field a = Field
  { fieldContent :: Row (IO Element)
  , fieldType    :: String
  , getValue     :: IO a
  , setValue     :: a -> IO ()
  }

dropdownField :: Dropdown -> String -> String -> IO (Field String)
dropdownField dd typ lab = do
  (sel,getVal,setVal) <- toElementAction dd
  return $ Field
    (labeledField lab $ element sel)
    typ
    getVal
    setVal

radsField :: ToElements a => Radios a -> String -> String -> IO (Field (Maybe String))
radsField rs typ lab = do
  (rads,getVal,setVal) <- toElementsAction rs
  return $ Field
    (labeledField lab $ inlineList $ map element rads)
    typ
    getVal
    setVal

noteTextField :: String -> String -> String -> IO (Field String)
noteTextField typ lab note = do
  fld <- textField typ lab
  return $ fld
    { fieldContent = set placeholder note <$> fieldContent fld
    }

textField :: String -> String -> IO (Field String)
textField typ lab = do
  inp <- UI.input # set UI.type_ "text"
  return $ Field
    (labeledField lab (element inp))
    typ
    (get value inp)
    (\s -> void $ element inp # set value s)

labeledField :: String -> IO Element -> Row (IO Element)
labeledField lab elt = collapseRow
  [ uniformLayout (colWidth 3) $ inlineLabel lab
  , uniformLayout (colWidth 9) elt
  ]

inlineLabel :: String -> IO Element
inlineLabel lab = label # set classes ["inline"] #~ lab

inlineList :: [IO Element] -> IO Element
inlineList rs = UI.ul # set classes ["inline-list"] #+
  map (UI.li #+!) rs

-- }}}

