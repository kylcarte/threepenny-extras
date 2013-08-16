{-# LANGUAGE OverloadedStrings #-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

main :: IO ()
main = do
  startGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = ""
    } setup

data TestField = TestField Int String deriving (Eq,Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

setup :: Window -> IO ()
setup w = void $ do
  return w # set title "Page Switching"

  conn <- open "test.db"

  inp <- UI.input # set UI.name "TestField"
  (but1,but1view) <- mkButton "Add"
  tbl <- UI.table                #
           set UI.name  "Fields" #
           set UI.rules "rows"   #
           set UI.cellpadding 5

  on UI.click but1 $ \_ -> do
    v <- get value inp
    insertTestField conn v
    element inp # set value ""
    refreshList conn tbl

  refreshList conn tbl
  getBody w #+
    [ UI.h1 #+ [ string "Test DB" ]
    , row [ element inp , element but1view ]
    , element tbl
    ]

refreshList :: Connection -> Element -> IO ()
refreshList conn tbl = void $ do
  tfs <- getTestFields conn
  element tbl #~ mapM mkRow tfs
  where
  mkRow tf@(TestField idNo _) = do
    (but,butView) <- mkButton "Remove"
    on UI.click but $ \_ -> do
      deleteTestField conn idNo
      refreshList conn tbl
    mkTableRow
      [ string (show tf)
      , element but
      ]

-- DB Operations {{{

insertTestField  :: Connection -> String -> IO ()
insertTestField  conn str = execute conn "INSERT INTO test (str) VALUES (?)" (Only str)

getTestFields    :: Connection -> IO [TestField]
getTestFields    conn = query_ conn "SELECT * from test"

deleteTestField  :: Connection -> Int -> IO [TestField]
deleteTestField  conn idNo = query conn "DELETE from test where ID = ?" (Only idNo)

matchTestFields :: Connection -> String -> IO [TestField]
matchTestFields conn str = query conn "SELECT * from test where str LIKE ?" (Only $ "%" ++ str ++ "%")

searchTestFields :: Connection -> String -> IO [TestField]
searchTestFields conn str = query conn "SELECT * from test where str = ?" (Only str)

-- }}}

-- Helpers {{{

type Page = IO [Element]

page :: [IO Element] -> Page
page = sequence

onSelect :: (Element,M.Map String a) -> (a -> IO void) -> IO ()
onSelect (sel,selMap) f = on (domEvent "change") sel $ \_ -> do
  k <- get value sel
  whenJust (M.lookup k selMap) $ \v -> void $ f v

(#~) :: IO Element -> IO [Element] -> IO Element
e #~ msetup = do
  es <- msetup
  e # set children es

mkSelectionMap :: [(String,a)] -> IO (Element, M.Map String a)
mkSelectionMap optMap = do
  let (opts,vals) = unzip $ flip map (zip optMap [0..]) $
                      \((opt,val), i) ->
                        let ix = show i in ((opt,ix),(ix,val))
  sel <- mkDropDown opts
  let selMap = M.fromList vals
  return (sel,selMap)

mkTableRow :: [IO Element] -> IO Element
mkTableRow es = UI.tr #+ map mkTd es
  where
  mkTd e = UI.td #+ [e]

mkDropDown :: [(String,String)] -> IO Element
mkDropDown opts = UI.select #~ mapM mkOpt opts
  where
  mkOpt (opt,val) = UI.option # set html opt # set value val

mkButton :: String -> IO (Element, Element)
mkButton title = do
  button <- UI.button #+ [string title]
  view   <- UI.p #+ [element button]
  return (button, view)

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m

-- }}}

