{-# LANGUAGE OverloadedStrings #-}

module Library.DBTypes where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok
import Data.Time.Calendar

import System.Directory
import System.Exit
import System.Process

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Data.List (intercalate)

initDB :: FilePath -> IO Connection
initDB fp = do
  exists <- doesFileExist fp
  unless exists $ do
    putStrLn $ "Database " ++ show fp ++ " does not exist, creating it now."
    -- XXX totally system dependent. needs review for cross-platform use.
    ec <- system $ "sqlite3 " ++ fp ++ " \"\""
    case ec of
      ExitSuccess   -> return ()
      ExitFailure _ -> exitWith ec
  conn <- open fp
  checkPatronTable conn
  checkAlertTable conn
  checkItemTable conn
  return conn

-- Patron {{{

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

checkPatronTable :: Connection -> IO ()
checkPatronTable conn = do
  res <- query_ conn "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'patrons'"
  when (null (res :: [Only String])) $ do
    putStrLn "Creating patrons table."
    execute_ conn
      "CREATE TABLE patrons (ID INTEGER PRIMARY KEY, firstname TEXT NOT NULL, lastname TEXT NOT NULL, phonenumber TEXT NOT NULL, emailaddr TEXT NOT NULL, prefcont INTEGER NOT NULL, homeaddr1 TEXT NOT NULL, homeaddr2 TEXT NOT NULL, citystatezip TEXT NOT NULL, patronnum INTEGER NOT NULL)"

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

searchPatronsNum :: Connection -> Integer -> IO [Patron]
searchPatronsNum conn x = query conn
  "SELECT * FROM patrons WHERE patronnum = ?"
  (Only x)

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



-- Alert {{{

data Alert = Alert
  { alertId     :: Maybe Integer
  , alertMsg    :: String
  , alertPatron :: Maybe Integer  -- patronId
  }

instance FromRow Alert where
  fromRow = Alert
        <$> (Just <$> field)  -- alertId
        <*> field             -- alertMsg
        <*> field             -- alertPatron

mkAlert :: String -> Maybe Integer -> Alert
mkAlert = Alert Nothing

-- }}}

-- Alert DB {{{

checkAlertTable :: Connection -> IO ()
checkAlertTable conn = do
  res <- query_ conn "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'alerts'"
  when (null (res :: [Only String])) $ do
    putStrLn "Creating alerts table."
    execute_ conn
      "CREATE TABLE alerts (ID INTEGER PRIMARY KEY, message TEXT NOT NULL, patron INTEGER)"

insertAlert :: Connection -> Alert -> IO ()
insertAlert conn a = execute conn
  "INSERT INTO alerts VALUES (null, ?, ?)"
  ( alertMsg    a
  , alertPatron a
  )

getAlerts :: Connection -> IO [Alert]
getAlerts conn = query_ conn
  "SELECT * FROM alerts"

deleteAlert :: Connection -> Integer -> IO [Alert]
deleteAlert conn idNo = query conn
  "DELETE FROM alerts where ID = ?"
  (Only idNo)

-- ALL MATCH METHODS TAKE STRINGS
matchAlertsMsg :: Connection -> String -> IO [Alert]
matchAlertsMsg conn x = query conn
  "SELECT * FROM alerts WHERE message LIKE ?"
  (Only $ concat ["%",x,"%"])

searchAlertsId :: Connection -> Integer -> IO [Alert]
searchAlertsId conn x = query conn
  "SELECT * FROM alerts WHERE ID = ?"
  (Only x)

searchAlertsPatron :: Connection -> Integer -> IO [Alert]
searchAlertsPatron conn x = query conn
  "SELECT * FROM alerts WHERE patron = ?"
  (Only x)

-- }}}



-- Item {{{

data Item = Item
  { itemId  :: Maybe Integer
  , dueDate :: Date
  , patron  :: Integer
  , title   :: Maybe String
  } deriving (Eq,Show)

instance FromRow Item where
  fromRow = Item
        <$> (Just <$> field) -- itemId
        <*> field            -- dueDate
        <*> field            -- patron (id)
        <*> field            -- title

mkItem :: Date -> Integer -> Maybe String -> Item
mkItem = Item Nothing

-- Avoiding Orphan instances by wrapping in a newtype
newtype Date = Date { toDay :: Day } deriving (Eq,Ord,Show)

instance ToField Date where
  toField = toField . toModifiedJulianDay . toDay

instance FromField Date where
  fromField f = Date . ModifiedJulianDay <$> fromField f

-- }}}

-- Item DB {{{

checkItemTable :: Connection -> IO ()
checkItemTable conn = do
  res <- query_ conn "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'items'"
  when (null (res :: [Only String])) $ do
    putStrLn "Creating checked out items table."
    execute_ conn
      "CREATE TABLE items (ID INTEGER PRIMARY KEY, duedate TEXT NOT NULL, patron INTEGER NOT NULL, title TEXT)"

insertItem :: Connection -> Item -> IO ()
insertItem conn i = execute conn
  "INSERT INTO items VALUES (null, ?, ?, ?)"
  ( dueDate i
  , patron  i
  , title   i
  )

getItems :: Connection -> IO [Item]
getItems conn = query_ conn
  "SELECT * FROM items"

deleteItem :: Connection -> Integer -> IO [Item]
deleteItem conn idNo = query conn
  "DELETE FROM items where ID = ?"
  (Only idNo)

-- ALL MATCH METHODS TAKE STRINGS
matchItemsTitle :: Connection -> String -> IO [Item]
matchItemsTitle conn x = query conn
  "SELECT * FROM items WHERE title LIKE ?"
  (Only $ concat ["%",x,"%"])

matchItemsDate :: Connection -> String -> IO [Item]
matchItemsDate conn x = query conn
  "SELECT * FROM items WHERE duedate LIKE ?"
  (Only $ concat ["%",x,"%"])

matchItemsPatron :: Connection -> String -> IO [Item]
matchItemsPatron conn x = query conn
  "SELECT * FROM items WHERE patron LIKE ?"
  (Only $ concat ["%",x,"%"])

searchItemsId :: Connection -> Integer -> IO [Item]
searchItemsId conn x = query conn
  "SELECT * FROM items WHERE ID = ?"
  (Only x)

searchItemsDate :: Connection -> Date -> IO [Item]
searchItemsDate conn x = query conn
  "SELECT * FROM items WHERE duedate = ?"
  (Only x)

searchItemsPatron :: Connection -> Integer -> IO [Item]
searchItemsPatron conn x = query conn
  "SELECT * FROM items WHERE patron = ?"
  (Only x)

-- }}}

