
module Library.Page.PatronSearch where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (row)

import Foundation.Common

import Library
import Library.DBTypes
import Library.Page.Search

import Database.SQLite.Simple

import Control.Applicative hiding (optional)
import Control.Monad

type PatronSearch extra = 
     (Element,Element) -- drawArea, buttonArea
  -> Connection        -- DB Connection
  -> Patron            -- this Patron
  -> [Patron]          -- search results (for "Back", etc.)
  -> extra             -- whatever else is needed
  -> IO ()

-- Patron Search {{{

patronSearch' :: String -> Connection -> PatronSearch () -> Page
patronSearch' pg conn act = patronSearch pg conn act ()

patronSearch :: String -> Connection -> PatronSearch extra -> extra -> Page
patronSearch pageNm conn infoFn extra = search pageNm conn searchFn extra
  [ ( "Patron Number" , \sTerm -> case patNumValid sTerm of
                                    Right (Just n) -> Right
                                      <$> searchPatronsNum conn n
                                    _ -> return $ Left
                                           "Patron Number is malformed" )
  , ( "Last Name"     , \sTerm -> Right <$> searchPatronsLastName conn sTerm )
  , ( "Email Address" , \sTerm -> case emailValid sTerm of
                                    Right e -> Right
                                      <$> searchPatronsEmail conn e
                                    _ -> return $ Left "Email is malformed" )
  ]
  where
  searchFn es c ps x = displayPatronTable es c ps (infoFn,x)

-- }}}

-- Display Patron Table {{{

displayPatronTable' :: Search Patron (PatronSearch ())
displayPatronTable' es ps conn act =
  displayPatronTable es ps conn (act,())

displayPatronTable :: Search Patron (PatronSearch extra,extra)
displayPatronTable (drawArea,btnArea) conn ps (infoFn,extra) = void $ do
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

