
module Library.Page.NewPatron where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Foundation.Input
import Foundation.Layout
import Foundation.Common

import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec.Rfc2822 as P

import Control.Monad
import Data.Char  (isDigit)

-- New Patron {{{

newPatron :: (Label,IO [IO Element])
newPatron =
  ( LabelStr "New Patron"
  , do (fstNameFld , getFstName) <- textField "First Name*"
       (lstNameFld , getLstName) <- textField "Last Name*"
       (emailFld   , getEmail  ) <- textField "Email*"
       (phoneFld   , getPhone  ) <- textField "Phone No.*"
       (prefRads   , getPref   ) <- toElementsAction prefContRads
       (home1Fld   , getHome1  ) <- noteTextField "Address Line 1"
                                     "Street address, P.O. box"
       (home2Fld   , getHome2  ) <- noteTextField "Address Line 2"
                                     "Apartment, suite, unit, building, floor, etc."
       (cityFld    , getCity   ) <- textField "City"
       (stateDD    , getState  ) <- toElementAction statesDropdown
       (zipFld     , getZip    ) <- textField "ZIP"
       alertArea                 <- UI.div

       let inputFailure = setAlert alertArea
       let inputSuccess = setSuccess alertArea
       let validateInput = do
             fstName <- getFstName
             lstName <- getLstName
             email   <- getEmail
             phone   <- getPhone
             pref    <- getPref
             home1   <- getHome1
             home2   <- getHome2
             city    <- getCity
             state   <- getState
             zipCd   <- getZip
             validate         (notNull    fstName) (inputFailure "First Name"       ) $
               validate       (notNull    lstName) (inputFailure "Last Name"        ) $
                 validate     (emailValid email  ) (inputFailure "Email"            ) $
                   validate   (phoneValid phone  ) (inputFailure "Phone"            ) $
                     validate (havePref   pref   ) (inputFailure "Preferred Contact") $
                       void $ do
                         inputSuccess
                         {- insertPatron -}
       submitBtn <- toElement $ Button (LabelStr "Submit") radiusBtnStyle validateInput

       return
         [ toElement fstNameFld
         , toElement lstNameFld
         , toElement emailFld
         , toElement phoneFld
         , toElement $ labeledField "Preferred Contact*" $ inlineList $ map element prefRads
         , toElement home1Fld
         , toElement home2Fld
         , toElement cityFld
         , toElement $ labeledField "State" $ element stateDD
         , toElement zipFld
         , UI.h5 #+! (UI.small #~ "* Required Fields")
         , UI.hr
         , toElement $ paddedRow [uniformLayout (centered 10) (center #+! element alertArea)]
         , toElement $ paddedRow [uniformLayout (offset 9 3) (element submitBtn)]
         ]
  )
  where
  prefContRads = Radios "prefcont" False
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

setSuccess :: Element -> IO ()
setSuccess e = void $ do
  alrt <- UI.div #
    set (data_ "alert") "" #
    set classes ["alert-box","success","round"] #~
      "Patron Added!"
  element e # set children [ alrt ]

setAlert :: Element -> String -> String -> IO ()
setAlert e typ msg = void $ do
  alrt <- UI.div #
    set (data_ "alert") "" #
    set classes ["alert-box","alert","round"] #~
      ("*** " ++ msg ++ " " ++ typ ++ " ***")
  element e # set children [ alrt ]

validate :: Maybe String -> (String -> IO a) -> IO a -> IO a
validate testRes onFail onSuccess = maybe onSuccess onFail testRes

notNull :: [a] -> Maybe String
notNull as
  | null as   = Just "Missing"
  | otherwise = Nothing

emailValid :: String -> Maybe String
emailValid em = if null em
  then Just "Missing"
  else case parseEmail em of
         Left _  -> Just "Malformed"
         Right _ -> Nothing
  where
  parseEmail = P.parse P.addr_spec ""

phoneValid :: String -> Maybe String
phoneValid ph = if null ph
  then Just "Missing"
  else if all (\c -> isDigit c || elem c phoneNumAcceptableOtherChars) ph
    then Nothing
    else Just "Malformed"

phoneNumAcceptableOtherChars :: [Char]
phoneNumAcceptableOtherChars = " ()-"

havePref :: Maybe String -> Maybe String
havePref (Just _) = Nothing
havePref Nothing  = Just "Missing"

-- }}}

-- Helpers {{{

noteTextField :: String -> String -> IO (Row (IO Element),IO String)
noteTextField lab note = do
  inp <- UI.input # set UI.type_ "text" # set placeholder note
  return (labeledField lab (element inp), get value inp)

textField :: String -> IO (Row (IO Element),IO String)
textField lab = do
  inp <- UI.input # set UI.type_ "text"
  return (labeledField lab (element inp), get value inp)

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

