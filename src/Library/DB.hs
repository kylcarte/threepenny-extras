{-# LANGUAGE OverloadedStrings #-}

module Library.DB where

import Library.DB.Types

import Database.SQLite.Simple

import System.Directory
import System.Exit
import System.Process

import Control.Applicative
import Control.Monad

import qualified Data.Text as T

data DBPaths = DBPaths
  { patronsDBPath   :: FilePath
  , checkOutsDBPath :: FilePath
  , booksDBPath     :: FilePath
  }

data DB = DB
  { patronsDB   :: Connection
  , checkOutsDB :: Connection
  , booksDB     :: Connection
  }

initDBs :: DBPaths -> IO DB
initDBs ps = DB
  <$> initDB (patronsDBPath   ps) patronsTable
  <*> initDB (checkOutsDBPath ps) checkOutsTable
  <*> initDB (booksDBPath     ps) booksTable

initDB :: FilePath -> Table a -> IO Connection
initDB fp tbl = do
  exists <- doesFileExist fp
  unless exists $ do
    putStrLn $ "Database " ++ show fp ++ " does not exist, creating it now."
    -- XXX totally system dependent. needs review for cross-platform use.
    ec <- system $ "sqlite3 " ++ fp ++ " \"\""
    case ec of
      ExitSuccess   -> return ()
      ExitFailure _ -> exitWith ec
  conn <- open fp
  res <- query conn "SELECT name FROM sqlite_master WHERE type = ? AND name = ?" ("table" :: String,queryToString $ tableName tbl)
  when (null (res :: [Only String])) $ do
    putStrLn $ "Creating " ++ (queryToString $ tableName tbl) ++" table."
    execute_ conn $ mkTableCreate tbl
  return conn

queryToString :: Query -> String
queryToString = T.unpack . fromQuery

stringToQuery :: String -> Query
stringToQuery = read . ("\"" ++) . (++ "\"")

