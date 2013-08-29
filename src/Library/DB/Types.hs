{-# LANGUAGE OverloadedStrings #-}

module Library.DB.Types where

import Library.DB

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok

import Data.Time
import System.Locale

import System.Directory
import System.Exit
import System.Process

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Monoid

-- Patron {{{

mIntercalate :: Monoid m => m -> [m] -> m
mIntercalate s [] = mempty
mIntercalate s [m] = m
mIntercalate s (m:ms) = m <> s <> mIntercalate s ms 

unwordsQuery :: [Query] -> Query
unwordsQuery = mIntercalate " "

parensQuery :: Query -> Query
parensQuery q = "(" <> q <> ")"

mkTableCreate :: Query -> [(Query,Query)] -> Query
mkTableCreate tblName flds = unwordsQuery
  [ "CREATE TABLE"
  , tblName
  , parensQuery $
    mIntercalate ", "
    ( "ID INTEGER PRIMARY KEY"
    : map mkField flds
    )
  ]
  where
  mkField (fld,typ) = unwordsQuery [fld,typ,"NOT NULL"]

data Patron = Patron
  { patronDBId   :: Maybe Integer      -- Maybe, so we can construct Patrons
  , firstName    :: String               --  and insert them conveniently
  , lastName     :: String
  , phoneNumber  :: PhoneNumber
  , emailAddr    :: Email
  , prefContact  :: Contact
  , homeAddr1    :: Addr
  , homeAddr2    :: Addr
  , cityStateZip :: CityStateZip
  , patronNum    :: Integer
  } deriving (Eq,Show)

patronsTable :: Table
patronsTable = Table "patrons"
  "CREATE TABLE patrons \
     \( ID INTEGER PRIMARY KEY\
     \, firstname TEXT NOT NULL\
     \, lastname TEXT NOT NULL\
     \, phonenumber TEXT NOT NULL\
     \, emailaddr TEXT NOT NULL\
     \, prefcont INTEGER NOT NULL\
     \, homeaddr1 TEXT NOT NULL\
     \, homeaddr2 TEXT NOT NULL\
     \, citystatezip TEXT NOT NULL\
     \, patronnum INTEGER NOT NULL)"

instance FromRow Patron where
  fromRow = Patron
        <$> (Just <$> field) -- patronId
        <*> field <*> field  -- first, last names
        <*> field <*> field  -- phone, email
        <*> field            -- preferred means of contact
        <*> field <*> field  -- home address 1 & 2
        <*> field            -- cityStateZip
        <*> field            -- patronNum

type PhoneNumber = String
type Email = String
type Addr  = String

mkPatron :: String -> String
         -> PhoneNumber
         -> Email
         -> Contact
         -> Addr -> Addr
         -> CityStateZip
         -> Integer
         -> Patron
mkPatron = Patron Nothing

-- We support FromRow and *not* ToRow, because we will be inserting Patrons
-- with the sqlite-simple library's parameter substitution for fields, rather
-- than as a whole.

-- However, for abstraction's sake, the newtyped fields of Patron support
-- ToField behavior, so they do not need to be deconstructed at substitution time.

data Contact
  = Phone
  | Email
  deriving (Eq,Show)

instance FromField Contact where
  fromField f = fc =<< fromField f
    where
    fc :: Integer -> Ok Contact
    fc 0 = return Phone
    fc 1 = return Email
    fc n = fail $ "No parse for Contact: " ++ show n

instance ToField Contact where
  toField pr = toField n
    where
    n :: Integer
    n = case pr of
          Phone -> 0
          Email -> 1



data CityStateZip = CSZ
  { viewCity  :: String
  , viewState :: String
  , viewZipCd :: String
  } deriving (Eq,Show)

semiSep :: String -> Ok (String,String)
semiSep inp = case break (== ';') inp of
  (r,';':s') -> return (r,s')
  _          -> fail $ "Not semicolon separated: " ++ inp

instance FromField CityStateZip where
  fromField f = fcsz =<< fromField f
    where
    fcsz :: String -> Ok CityStateZip
    fcsz s0 = do (ct,s1) <- semiSep s0
                 (st,zp) <- semiSep s1
                 if  length zp == 0 ||
                    (length zp == 5 && all isDigit zp)
                   then return $ CSZ ct st zp
                   else fail $ "Malformed Zipcode: " ++ zp

instance ToField CityStateZip where
  toField (CSZ c s z) = toField $ intercalate ";" [c,s,z]

-- }}}

-- Patron DB {{{

insertPatron :: Connection -> Patron -> IO ()
insertPatron conn p = execute conn
  "INSERT INTO patrons VALUES (null, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  ( firstName    p
  , lastName     p
  , phoneNumber  p
  , emailAddr    p
  , prefContact  p
  , homeAddr1    p
  , homeAddr2    p
  , cityStateZip p
  , patronNum    p
  )

updatePatron :: Connection -> Patron -> IO ()
updatePatron conn p = case patronDBId p of
  Nothing   -> fail "Bug: Tried to update patron without providing database ID"
  Just idNo -> execute conn
    "UPDATE patrons SET firstname = ?, lastname = ?, phonenumber = ?, emailaddr = ?, prefcont = ?, homeaddr1 = ?, homeaddr2 = ?, citystatezip = ?, patronnum = ? WHERE ID = ?"
    ( firstName    p
    , lastName     p
    , phoneNumber  p
    , emailAddr    p
    , prefContact  p
    , homeAddr1    p
    , homeAddr2    p
    , cityStateZip p
    , patronNum    p
    , idNo
    )

getPatrons :: Connection -> IO [Patron]
getPatrons conn = query_ conn
  "SELECT * FROM patrons"

-- [(patronNum,patronDBId)]
getPatronNumbers :: Connection -> IO [(Integer,Integer)]
getPatronNumbers conn = query_ conn
  "SELECT patronnum,ID FROM patrons"

deletePatron :: Connection -> Integer -> IO [Patron]
deletePatron conn idNo = query conn
  "DELETE FROM patrons WHERE ID = ?"
  (Only idNo)

-- ALL MATCH METHODS TAKE STRINGS
matchPatronsFirstName :: Connection -> String -> IO [Patron]
matchPatronsFirstName conn x = query conn
  "SELECT * FROM patrons WHERE firstname LIKE ?"
  (Only $ concat ["%",x,"%"])

matchPatronsLastName :: Connection -> String -> IO [Patron]
matchPatronsLastName conn x = query conn
  "SELECT * FROM patrons WHERE lastname LIKE ?"
  (Only $ concat ["%",x,"%"])

-- There should be at most one patron with a given number
searchPatronsNumber :: Connection -> Integer -> IO (Maybe Patron)
searchPatronsNumber conn x = do
  ps <- query conn
          "SELECT * FROM patrons WHERE patronnum = ?"
          (Only x)
  case ps of
    [] -> return Nothing
    [p] -> return $ Just p
    _   -> fail $ "Bug: more than one patron with number " ++ show x

searchPatronsLastName :: Connection -> String -> IO [Patron]
searchPatronsLastName conn x = query conn
  "SELECT * FROM patrons WHERE lastname = ?"
  (Only x)

searchPatronsEmail :: Connection -> String -> IO [Patron]
searchPatronsEmail conn x = query conn
  "SELECT * FROM patrons WHERE emailaddr = ?"
  (Only x)

patronNumberInDB :: Connection -> Integer -> IO (Maybe Integer)
patronNumberInDB conn patNum = do
  ns <- getPatronNumbers conn
  return $ lookup patNum ns

patronNumberTaken :: Connection -> Integer -> Integer -> IO Bool
patronNumberTaken conn idNo patNum =
      maybe False (not . (idNo ==))
  <$> patronNumberInDB conn patNum

-- }}}



-- Book {{{

booksTable :: Table
booksTable = Table undefined undefined

-- }}}



-- CheckOut {{{

data CheckOut = CheckOut
  { checkOutDBId      :: Maybe Integer
  , checkOutPatronId  :: Integer
  , checkOutBookId    :: Integer
  , checkOutTimeStamp :: TimeStamp
  } deriving (Eq,Show)

checkOutsTable :: Table
checkOutsTable = Table "checkouts"
  "CREATE TABLE checkouts \
     \( ID INTEGER PRIMARY KEY\
     \, patronid INTEGER NOT NULL\
     \, bookid INTEGER NOT NULL\
     \, timestamp TEXT NOT NULL)"

instance FromRow CheckOut where
  fromRow = CheckOut
        <$> (Just <$> field) -- checkOutDBId
        <*> field            -- checkOutPatronId
        <*> field            -- checkOutBookId
        <*> field            -- checkOutTimeStamp

mkCheckOut :: Integer -> Integer -> TimeStamp -> CheckOut
mkCheckOut = CheckOut Nothing

-- }}}

-- CheckOut DB {{{

checkCheckOutTable :: Connection -> IO ()
checkCheckOutTable conn = do
  res <- query_ conn "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'checkouts'"
  when (null (res :: [Only String])) $ do
    putStrLn "Creating checkouts table."
    execute_ conn
      "CREATE TABLE checkouts (ID INTEGER PRIMARY KEY, patronid INTEGER NOT NULL, bookid INTEGER NOT NULL, timestamp TEXT NOT NULL)"

insertCheckOut :: Connection -> CheckOut -> IO ()
insertCheckOut conn c = execute conn
  "INSERT INTO checkouts VALUES (null, ?, ?, ?)"
  ( checkOutPatronId  c
  , checkOutBookId    c
  , checkOutTimeStamp c
  )

getCheckOuts :: Connection -> IO [CheckOut]
getCheckOuts conn = query_ conn
  "SELECT * FROM checkouts"

deleteCheckOut :: Connection -> Integer -> IO [CheckOut]
deleteCheckOut conn idNo = query conn
  "DELETE FROM checkouts where ID = ?"
  (Only idNo)

matchCheckOutsMDY :: Connection -> (Integer,Integer,Integer) -> IO [CheckOut]
matchCheckOutsMDY conn mdy = do
  cs <- map parseInfo <$> getCheckOuts conn
  return $
    map snd $
    filter ((== mdy) . fst) cs
  where
  parseInfo c = (parseMDY c,c)
  parseMDY = parseTimeFields . formatTimeStamp . checkOutTimeStamp
  formatTimeStamp = formatTime defaultTimeLocale "%m%d%C%Y" . timeStamp
  parseTimeFields :: String -> (Integer,Integer,Integer)
  parseTimeFields l@[m1,m2,d1,d2,y1,y2,y3,y4]
    | all isDigit l = (read [m1,m2],read [d1,d2],read [y1,y2,y3,y4])
  parseTimeFields s = error $ "Bad time string? " ++ s

searchCheckOutsId :: Connection -> Integer -> IO [CheckOut]
searchCheckOutsId conn x = query conn
  "SELECT * FROM checkouts WHERE ID = ?"
  (Only x)

searchCheckOutsPatronId :: Connection -> Integer -> IO [CheckOut]
searchCheckOutsPatronId conn x = query conn
  "SELECT * FROM checkouts WHERE patronid = ?"
  (Only x)

searchCheckOutsPatronNum :: Connection -> Integer -> IO [CheckOut]
searchCheckOutsPatronNum conn x = do
  mp <- searchPatronsNumber conn x
  case mp of
    Nothing -> return []
    Just p  -> case patronDBId p of
      Nothing -> fail $ "Bug: Patron retrieved from DB has no Id: "
                   ++ show (patronNum p)
      Just pn -> searchCheckOutsPatronId conn pn

searchCheckOutsBookId :: Connection -> Integer -> IO [CheckOut]
searchCheckOutsBookId conn x = query conn
  "SELECT * FROM checkouts WHERE bookid = ?"
  (Only x)

-- }}}



-- TimeStamp {{{

newtype TimeStamp = TimeStamp
  { timeStamp :: UTCTime
  } deriving (Eq,Show)

dbTimeFormat :: String
dbTimeFormat = "%m %d %C%Y %H %M"

instance ToField TimeStamp where
  toField =
    toField .
    formatTime defaultTimeLocale dbTimeFormat .
    timeStamp

instance FromField TimeStamp where
  fromField f = do
    txt <- fromField f
    let mt = parseTime
               defaultTimeLocale
               dbTimeFormat
               txt
    case mt of
      Nothing -> fail $ "Malformed UTCTime: " ++ txt
      Just t  -> return $ TimeStamp t

-- }}}

