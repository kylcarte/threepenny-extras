
module Library.Page.PatronInfo where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Foundation.Common
import Foundation.Input
import Foundation.Layout
import Foundation.Sections (Page)

import Library
import Library.DBTypes

import Database.SQLite.Simple

import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec.Rfc2822 as P
import System.Random

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Data.List (elemIndex)

-- Patron Info {{{

patronInfo :: Connection
           -> (   Connection          -- DB Connection
               -> Element             -- Alert Area
               -> (Integer -> Patron) -- All other information bundled
                                      --   just needing a patron number
               -> Maybe Integer       -- Patron Number from Form
               -> IO ())
           -> Page
patronInfo conn patronAction = do
  -- Make Fields
  fstNameFld         <- textField "First Name"        "First Name*"
  lstNameFld         <- textField "Last Name"         "Last Name*"
  emailFld           <- textField "Email Address"     "Email*"
  phoneFld           <- textField "Phone Number"      "Phone No.*"
  prefFld            <- radsField prefRads
                                  "Preferred Contact" "Preferred Contact*"
  home1Fld           <- noteTextField "Home Addr1"    "Address Line 1"
                          "Street address, P.O. box"
  home2Fld           <- noteTextField "Home Addr2"    "Address Line 2"
                          "Apartment, suite, unit, building, floor, etc."
  cityFld            <- textField "City" "City"
  stateFld           <- dropdownField statesDropdown "State" "State"
  zipFld             <- textField "Zip Code" "ZIP"
  patNumFld          <- noteTextField "Patron Number" "Existing Patron Number"
                          "Only needed if adding an existing patron!"

  alertArea          <- UI.div
  let validate = validateField alertArea
  submitBtn <- toElement $ 
    Button (LabelStr "Submit") radiusBtnStyle $
      -- Validate Input
      validate fstNameFld notNull     $ \fstName ->
      validate lstNameFld notNull     $ \lstName ->
      validate emailFld   emailValid  $ \email   ->
      validate phoneFld   phoneValid  $ \phone   ->
      validate prefFld    havePref    $ \pref    ->
      validate zipFld     zipValid    $ \zipCd   ->
      validate patNumFld  patNumValid $ \mpn     -> do
        -- Input Validation Success
        home1   <- getValue home1Fld
        home2   <- getValue home2Fld
        city    <- getValue cityFld
        state   <- getValue stateFld
        -- Validate Patron Number with DB
        let mkPat pn = mkPatron pn
              fstName lstName
              phone email pref
              home1 home2
              (CSZ city state zipCd)
        patronAction conn alertArea mkPat mpn

  return
    [ render fstNameFld , render lstNameFld
    , render emailFld   , render phoneFld   , render prefFld
    , render home1Fld   , render home2Fld
    , render cityFld    , render stateFld   , render zipFld
    , UI.hr
    , render patNumFld
    , UI.h5 #+ [ UI.small #~ "* Required Fields" ]
    , UI.hr
    , toElement $ paddedRow
      [ uniformLayout (colWidth 9) $ toElement $ paddedRow
        [ uniformLayout (centered 10) $ center #+! element alertArea
        ]
      , uniformLayout (colWidth 3) $
        element submitBtn
      ]
    ]
  where
  prefRads = Radios "prefcont" False
    [ ( string "Email" # set UI.valign "middle" , "Email" )
    , ( string "Phone" # set UI.valign "middle" , "Phone" )
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

validateField :: Element -> Field a -> (a -> Validate res) -> (res -> IO ()) -> IO ()
validateField e fld test onSuccess = do
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
  ns <- getPatronNumbers conn
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

render :: Field a -> IO Element
render = toElement . fieldContent

dropdownField :: Dropdown -> String -> String -> IO (Field String)
dropdownField dd typ lab = do
  (sel,getVal) <- toElementAction dd
  let opts = map optString $ dropdownOpts dd
  let setVal o = void $
    -- (+ 1) because Dropdown adds an empty option at the top.
    -- If o is not found in the options, then we just reset the dropdown.
        element sel # set UI.selection ((+ 1) <$> elemIndex o opts)
  return $ Field
    (labeledField lab $ element sel)
    typ
    getVal
    setVal

radsField :: ToElements a => Radios a -> String -> String -> IO (Field (Maybe String))
radsField rs typ lab = do
  (rads,getVal) <- toElementsAction rs
  let setVal ms = forM_ rads $ \r -> do
                    v <- Just <$> get value r
                    element r # set UI.checked (v == ms)
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

