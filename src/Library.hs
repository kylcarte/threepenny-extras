
module Library where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (row)

import Foundation.Common
import Foundation.Input
import Foundation.Layout

import Library.DBTypes

import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec.Rfc2822 as P

import Database.SQLite.Simple
import System.Random hiding (split)

import Control.Applicative hiding (optional)
import Control.Monad
import Data.Char (isDigit)

numDigitsPatron :: Int
numDigitsPatron = 6

type Page = IO [IO Element]

-- Fields {{{

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
    -- XXX: This is applying the placeholder attr
    -- to both the label and the input box
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

optional :: (String -> String -> IO (Field a))
         -> String
         -> IO (Field a)
optional f typ = f typ typ
required :: (String -> String -> IO (Field a))
         -> String
         -> IO (Field a)
required f typ = f typ (typ ++ "*")

-- }}}

-- Input Validation {{{

type Validate a = Either String a
vdtSucc :: a -> Validate a
vdtSucc = Right

failMissing :: Validate a
failMissing = Left "Missing"

failMalformed :: Validate a
failMalformed = Left "Malformed"

validateWithArea :: Element -> Field a -> (a -> Validate res) -> (res -> IO ()) -> IO ()
validateWithArea e fld test onSuccess = do
  v <- getValue fld
  case test v of
    Left  err -> displayFailure e (err ++ " " ++ fieldType fld)
    Right res -> onSuccess res

-- Names
notNull :: String -> Validate String
notNull s
  | null s   = failMissing
  | otherwise = vdtSucc s

-- Email
emailValid :: String -> Validate String
emailValid em
  | null em = failMissing
  | otherwise = case parseEmail em of
      Left _  -> failMalformed
      Right _ -> vdtSucc em
  where
  parseEmail = P.parse P.addr_spec ""

-- Phone Number
phoneValid :: String -> Validate String
phoneValid ph
  | null ph = failMissing
  | all (\c -> isDigit c || elem c phoneAcceptableNonDigits) ph
    = vdtSucc $ filter isDigit ph
  | otherwise = failMalformed

phoneAcceptableNonDigits :: String
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
  | otherwise = failMalformed

-- Preferred Contact
havePref :: Maybe String -> Validate Contact
havePref (Just "Email") = vdtSucc Email
havePref (Just "Phone") = vdtSucc Phone
havePref (Just _)       = failMalformed
havePref Nothing        = failMissing

-- Patron Number
patNumValid :: String -> Validate (Maybe Integer)
patNumValid n
  | null n    = vdtSucc Nothing
  | length n == numDigitsPatron && all isDigit n
              = vdtSucc $ Just $ read n
  | otherwise = failMalformed

-- MM/DD/YYYY
mdyValid :: String -> Validate (Integer,Integer,Integer)
mdyValid [m1,m2,'/',d1,d2,'/',y1,y2,y3,y4]
  | all isDigit [m1,m2,d1,d2,y1,y2,y3,y4]
    = return (read [m1,m2],read [d1,d2],read [y1,y2,y3,y4])
  | otherwise = failMalformed
mdyValid "" = failMissing
mdyValid _  = failMalformed

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

