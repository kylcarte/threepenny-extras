
module Library.Page.CheckOutSearch where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (row)

import Foundation.Common

import Library
import Library.DBTypes
import Library.Page.Search

import Database.SQLite.Simple

import Control.Applicative hiding (optional)
import Control.Monad

type CheckOutSearch extra =
     (Element,Element) -- drawArea, buttonArea
  -> Connection        -- DB Connection
  -> CheckOut          -- this CheckOut
  -> [CheckOut]        -- search results (for "Back", etc.)
  -> extra             -- whatever else is needed
  -> IO ()

-- CheckOut Search {{{

checkOutSearch' :: String -> Connection -> CheckOutSearch () -> Page
checkOutSearch' pg conn act = checkOutSearch pg conn act ()

checkOutSearch :: String -> Connection -> CheckOutSearch extra -> extra -> Page
checkOutSearch pageNm conn infoFn extra = search pageNm conn searchFn extra
  [ ( "Patron Number" , \sTerm -> case patNumValid sTerm of
                                    Right (Just n) -> Right
                                      <$> searchCheckOutsPatronNum conn n
                                    _ -> return $ Left
                                           "Patron Number Malformed" )
  , ( "MM/DD/YYYY", \sTerm -> case mdyValid sTerm of
                                Right mdy -> Right
                                  <$> matchCheckOutsMDY conn mdy
                                Left err -> return $
                                  Left $ "Date Malformed: use MM/DD/YYYY format")
  ]
  where
  searchFn es cn cs x = displayCheckOutTable es cn cs (infoFn,x)

-- }}}

-- Display CheckOut Table {{{

displayCheckOutTable' :: Search CheckOut (CheckOutSearch ())
displayCheckOutTable' es ps conn act =
  displayCheckOutTable es ps conn (act,())

displayCheckOutTable :: Search CheckOut (CheckOutSearch extra,extra)
displayCheckOutTable (drawArea,btnArea) conn ps (infoFn,extra) = void $ do
  tbl <- UI.table # set widthPerc "100" #+
           [ thead #+! tableHdr
           , tbody #+ map tableRow ps
           ]
  element drawArea # set children [ tbl ]
  where
  th s = UI.th #~ s
  td e = UI.td #+! e
  tableHdr = UI.tr #+
    [ th "First"
    , th "Last"
    , th "Phone"
    , th "Email"
    , th "Patron #"
    ]
  patLink p s = do
    a <- UI.a # set UI.href "#"
    on UI.click a $ const $ do
      infoFn (drawArea,btnArea) conn p ps extra
    element a #~ s
  tableRow p = UI.tr #+
    [ td $ patLink p $        firstName   p
    , td $ patLink p $        lastName    p
    , td $ patLink p $        phoneNumber p
    , td $ patLink p $        emailAddr   p
    , td $ patLink p $ show $ patronNum   p
    ]

-- }}}

