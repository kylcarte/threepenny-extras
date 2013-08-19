
import DBTypes
import Input
import Utils

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Database.SQLite.Simple

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (catMaybes,fromMaybe,isJust)

main :: IO ()
main = do
  startGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = ""
    } setup

setup :: Window -> IO ()
setup w = void $ do
  conn <- initDB "library.db"
  getBody w #+ [ newPatronPage conn ]

newPatronPage :: Connection -> IO Element
newPatronPage conn = do
  fnm <- UI.input
  lnm <- UI.input
  (_,vs,getPrefCont) <- mkRadios "pc" [("Phone","0"),("Email","1")]
  ph  <- UI.input
  em  <- UI.input
  hm  <- UI.input
  (b,bv) <- mkButton "Add Patron"
  tbl <- mkTable
          [ [ string "First Name"        , element fnm ]
          , [ string "Last Name"         , element lnm ]
          , [ string "Phone Number"      , element ph  ]
          , [ string "Email Address"     , element em  ]
          , [ string "Home Address"      , element hm  ]
          , [ string "Preferred Contact" , mkTable [ map element vs ] ]
          , [ element bv ]
          ]
  entryPage <- UI.div #+ [ element tbl ]
  listPage  <- UI.div #+ [ mkPatronsPage conn ]
  wholePage <- UI.table #+ [ mkTableRow [ element entryPage , element listPage ] # set UI.valign "top" ]
  badStr <- string "Please fill in all fields."
  goodStr <- string "Patron added."
  on UI.click b $ const $ void $ do
    vs@[f,l,p,e,h] <- getValuesList [fnm,lnm,ph,em,hm]
    ci <- getPrefCont
    if (not (any null vs) && isJust ci)
    then do
      c <- case ci of
             Just "0" -> return (Phone,Email)
             Just "1" -> return (Email,Phone)
             _        -> fail $ "No parse for Preferred Contact: " ++ show ci
      let pat = mkPatron f l c p e h
      insertPatron conn pat
      element entryPage # set children [ tbl , goodStr ]
      ppg <- mkPatronsPage conn
      element listPage  # set children [ ppg ]
    else element entryPage # set children [ tbl , badStr ]
  element wholePage

mkPatronsPage :: Connection -> IO Element
mkPatronsPage conn = do
  pats <- getPatrons conn
  UI.table #+
    ( patTblHdr
    : map mkPatRow pats
    ) # set UI.border 1 # set UI.rules "rows" # set UI.cellpadding 5
  where
  patTblHdr =
    mkTableRow
      [ string "ID"
      , string "First Name"
      , string "Last Name"
      , string "Preferred Contact"
      , string "Phone Number"
      , string "Email Address"
      , string "Home Address"
      ]
  mkPatRow (Patron idNo fn ln c1 _ pn em hm) =
    mkTableRow
      [ string $ maybe "N/A" show idNo
      , string $ T.unpack fn
      , string $ T.unpack ln
      , string $ show c1
      , string $ maybe "" (T.unpack . fromPhoneNumber) pn
      , string $ maybe "" T.unpack em
      , string $ maybe "" T.unpack hm
      ]


-- Helpers {{{

mkTable :: [[IO Element]] -> IO Element
mkTable ess = UI.table #+ map mkTableRow ess

mkTableRow :: [IO Element] -> IO Element
mkTableRow es = UI.tr #+ map mkTd es
  where
  mkTd e = UI.td #+ [e]

-- }}}

