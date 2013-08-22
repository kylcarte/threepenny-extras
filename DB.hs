{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Foundation.Input
import Graphics.UI.Threepenny.Foundation.Layout
import Common

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = do
  foundationGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = "static/"
    } setup

setup :: Window -> IO ()
setup w = void $ do
  getBody w #+ toElements mainPage

mainPage :: Row (IO Element)
mainPage = collapseRow
  [ stackOnSmall (centered 6) $
    newPatron
  ]

-- New Patron {{{

newPatron :: IO Element
newPatron = do
  let (fstNameFld,getFstName) = textField "First Name*"
      (lstNameFld,getLstName) = textField "Last Name*"
      (emailFld  ,getEmail)   = textField "Email*"
      (phoneFld  ,getPhone)   = textField "Phone No.*"
      (home1Fld  ,getHome1)   = noteTextField "Address Line 1"
                                  "Street address, P.O. box"
      (home2Fld  ,getHome2)   = noteTextField "Address Line 2"
                                  "Apartment, suite, unit, building, floor, etc."
      (cityFld   ,getCity)    = textField "City"
      (zipFld    ,getZip)     = textField "ZIP"
  (prefRads  ,getPref)       <- toElementsAction prefContRads
  (stateDD  ,getState)       <- toElementAction statesDropdown
  panel #+
    [ UI.h5 #~ "New Patron"
    , toElement fstNameFld
    , toElement lstNameFld
    , toElement emailFld
    , toElement phoneFld
    , toElement $ prefContFld prefRads
    , toElement home1Fld
    , toElement home2Fld
    , toElement cityFld
    , toElement $ labeledField "State" (element stateDD)
    , toElement zipFld
    , string "* Required Fields"
    ]
  where
  prefContFld rs = labeledField "Preferred Contact" $
    UI.ul #
    set classes ["inline-list"] #+
      map ((UI.li #+!) . element) rs
  prefContRads = Radios "prefcont" False
    [ ( string "Email" # set UI.valign "middle" , "Email" )
    , ( string "Phone" # set UI.valign "middle" , "Phone" )
    ]

noteTextField :: String -> String -> (Row (IO Element),IO String)
noteTextField lab note =
  let inp = UI.input # set UI.type_ "text" # set placeholder note
  in
  (labeledField lab inp, get value =<< inp)

textField :: String -> (Row (IO Element),IO String)
textField lab = let inp = UI.input # set UI.type_ "text"
  in
  (labeledField lab inp, get value =<< inp)

labeledField :: String -> IO Element -> Row (IO Element)
labeledField lab elt = collapseRow
  [ uniformLayout (colWidth 3) $ inlineLabel lab
  , uniformLayout (colWidth 9) elt
  ]

statesDropdown :: Dropdown
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

inlineLabel :: String -> IO Element
inlineLabel lab = label # set classes ["inline"] #~ lab

