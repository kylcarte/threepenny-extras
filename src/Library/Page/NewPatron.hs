
module Library.Page.NewPatron where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Foundation.Common
import Foundation.Input
import Foundation.Layout
import Foundation.Sections (Page)

import Library.DBTypes

import Database.SQLite.Simple

import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec.Rfc2822 as P
import System.Random

import Control.Monad
import Data.Char  (isDigit)

numDigitsPatron :: Int
numDigitsPatron = 6

-- New Patron {{{

newPatron :: Connection -> Page
newPatron conn =
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
       (patNumFld  , getPatNum ) <- noteTextField "Existing Patron Number"
                                      "Only needed if adding an existing patron!"
       alertArea                 <- UI.div

       let inputFailure = setAlert alertArea
       let inputSuccess = setSuccess alertArea
       let inputResult  = setResult alertArea
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
             patNum  <- getPatNum
          -- Validation          | Acceptable Condition  | Failure Action
             validate                notNull     fstName (inputFailure "First Name"       ) $
               validate              notNull     lstName (inputFailure "Last Name"        ) $
                 validate            emailValid  email   (inputFailure "Email Address"    ) $
                   validateOut       phoneValid  phone   (inputFailure "Phone Number"     ) $ \ph ->
                     validateOut     havePref    pref    (inputFailure "Preferred Contact") $ \cont ->
                       validateOut   zipValid    zipCd   (inputFailure "Zip Code"         ) $ \zp ->
                         validateOut patNumValid patNum  (inputFailure "Patron Number"    ) $ \mpn ->
                           void $ do
                          -- Validation Success
                             mpn' <- case mpn of
                               -- New Patron
                               Nothing -> do
                                 n <- genPatronNum conn
                                 inputResult n
                                 return $ Just n
                               -- Existing Patron
                               Just n  -> do
                                 ns <- getPatronNumbers conn
                                 if n `elem` ns
                                   then do
                                     inputFailure "Patron Number" "Already Existing"
                                     return Nothing
                                   else do
                                     inputSuccess
                                     return $ Just n
                             whenJust mpn' $ \pn -> do
                               let pat = mkPatron pn
                                          fstName lstName
                                          ph email cont
                                          home1 home2
                                          (CSZ city state zp)
                               putStrLn $ "Adding Patron: " ++ fstName ++ " " ++ lstName ++ " : " ++ show pn
                               insertPatron conn pat

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
         , UI.hr
         , toElement patNumFld
         , UI.h5 #+! (UI.small #~ "* Required Fields")
         , UI.hr
         , toElement $ paddedRow
           [ uniformLayout (colWidth 9) $ toElement $ paddedRow
             [ uniformLayout (centered 10) $ center #+! element alertArea
             ]
           , uniformLayout (colWidth 3) $
             element submitBtn
           ]
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

-- }}}

-- Success/Failure Alerts {{{

setSuccess :: Element -> IO ()
setSuccess e = void $ do
  alrt <- alertBox ["success","round"] "Patron Added!"
  element e # set children [ alrt ]

setResult :: Element -> Integer -> IO ()
setResult e n = void $ do
  alrt <- toElement $ calloutPanel
            [ center #+! (UI.h5 #~ "New Patron Number")
            , center #+! (UI.h1 #~ show n)
            ]
  element e # set children [ alrt ]

setAlert :: Element -> String -> String -> IO ()
setAlert e typ msg = void $ do
  alrt <- alertBox ["alert","round"] (msg ++ " " ++ typ)
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

-- only validate
validate :: (a -> Maybe String) -> a -> (String -> IO b) -> IO b -> IO b
validate test a onFail onSuccess = maybe onSuccess onFail $ test a

-- validate with some sanitized result
validateOut :: (a -> Either String res) -> a -> (String -> IO b) -> (res -> IO b) -> IO b
validateOut test a onFail onSuccess = either onFail onSuccess $ test a

-- Names
notNull :: [a] -> Maybe String
notNull as
  | null as   = Just "Missing"
  | otherwise = Nothing

-- Email
emailValid :: String -> Maybe String
emailValid em = if null em
  then Just "Missing"
  else case parseEmail em of
         Left _  -> Just "Malformed"
         Right _ -> Nothing
  where
  parseEmail = P.parse P.addr_spec ""

-- Phone Number
phoneValid :: String -> Either String String
phoneValid ph = if null ph
  then Left "Missing"
  else if all (\c -> isDigit c || elem c phoneNumAcceptableOtherChars) ph
    then Right $ filter isDigit ph
    else Left "Malformed"

phoneNumAcceptableOtherChars :: [Char]
phoneNumAcceptableOtherChars = " ()-"

-- Zip Code
zipValid :: String -> Either String String
zipValid zp
  | null zp   = Right ""
  | length zp == 5 && all isDigit zp = Right zp
  | length zp == 10 &&
    zp !! 5 == '-' && 
    all isDigit (take 5 zp) &&
    all isDigit (take 4 $ reverse zp) = Right $ take 5 zp
  | otherwise = Left "Malformed"

-- Preferred Contact
havePref :: Maybe String -> Either String Contact
havePref (Just "Email") = Right Email
havePref (Just "Phone") = Right Phone
havePref (Just _)       = Left "Malformed"
havePref Nothing        = Left "Missing"

-- Patron Number
patNumValid :: String -> Either String (Maybe Integer)
patNumValid n
  | null n        = Right Nothing
  | length n == numDigitsPatron && all isDigit n
    = Right $ Just $ read n
  | otherwise     = Left "Malformed"

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

