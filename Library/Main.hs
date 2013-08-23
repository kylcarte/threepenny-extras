{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Foundation.Input
import Foundation.Layout
import Foundation.Sections
import Foundation.Common

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Text.ParserCombinators.Parsec.Rfc2822 as P

import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  foundationGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = "static/"
    } setup

setup :: Window -> IO ()
setup w = void $ do
  p <- toElement $ mainPage w
  getBody w #+ [ element p ]

mainPage :: Window -> Row (IO Element)
mainPage w = collapseRow
  [ stackOnSmall (centered 6) $
    toElement $
    Sections "Tabs" Tabs
      [ newPatron
      ]
  ]

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
       resultArea                <- UI.textarea
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
             validate         (notNull  fstName) (setAlert alertArea "First Name"       ) $
               validate       (notNull  lstName) (setAlert alertArea "Last Name"        ) $
                 validate     (emailBad email  ) (setAlert alertArea "Email"            ) $
                   validate   (phoneBad phone  ) (setAlert alertArea "Phone"            ) $
                     validate (havePref pref   ) (setAlert alertArea "Preferred Contact") $
                       void $ do
                         element alertArea # set children []
                         element resultArea # set value (unwords
                                   [ fstName , lstName
                                   , email   , phone   , fromMaybe "N/A" pref
                                   , home1   , home2
                                   , city    , state   , zipCd
                                   ])
       let submitBtn = Button
             { buttonLabel  = LabelStr "Submit"
             , buttonStyle  = radiusBtnStyle
             , buttonAction = validateInput 
             }
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
         , toElement submitBtn
         , element resultArea
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

clearAlert :: Element -> IO ()
clearAlert e = void $ element e # set children []

setAlert :: Element -> String -> String -> IO ()
setAlert e typ msg = void $ do
  alrt <- UI.div #
    set (data_ "alert") "" #
    set classes ["alert-box","alert","round"] #~
      ("*** " ++ msg ++ " " ++ typ ++ " ***")
  element e # set children
    [ alrt
    ]

{-
<div data-alert class="alert-box">
  <!-- Your content goes here -->
    <a href="#" class="close">&times;</a>
    </div>
-}

validate :: Maybe String -> (String -> IO a) -> IO a -> IO a
validate testRes onFail onSuccess = maybe onSuccess onFail testRes

notNull :: [a] -> Maybe String
notNull as
  | null as   = Just "Missing"
  | otherwise = Nothing

emailBad :: String -> Maybe String
emailBad em = if null em
  then Just "Missing"
  else case parseEmail em of
         Left err -> Just "Malformed"
         Right _  -> Nothing
  where
  parseEmail = P.parse P.addr_spec ""

phoneBad :: String -> Maybe String
phoneBad ph = if null ph
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

onMaybe :: Maybe a -> b -> (a -> b) -> b
onMaybe m b f = maybe b f m

-- }}}

