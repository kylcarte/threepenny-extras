
module Library.Page.PatronSearch where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Foundation.Common
import Foundation.Input
import Foundation.Layout
import Foundation.Sections (Page)

import Library.DBTypes
import Library.Page.PatronInfo

import Database.SQLite.Simple

import Control.Applicative hiding (optional)
import Control.Monad

type PatronSearch extra = 
     Element    -- area
  -> Connection -- DB Connection
  -> Patron     -- this Patron
  -> [Patron]   -- search results (for "Back", etc.)
  -> extra      -- whatever else is needed
  -> IO ()

-- Patron Search {{{

patronSearch :: Connection -> PatronSearch extra -> extra -> Page
patronSearch conn infoFn extra = do
  searchTypeFld <- optional (radsField searchRads) "Search By"
  searchFld     <- optional textField ""
  resArea       <- UI.div
  searchBtn     <- toElement $
    Button (LabelStr "Search") radiusBtnStyle $
      do sTerm <- getValue searchFld
         if null sTerm
           then displayFailure resArea "You must enter a search term"
           else do
             sType <- getValue searchTypeFld
             sRes <- case sType of
               Just "Patron Number" -> case patNumValid sTerm of
                                         Right (Just n) -> Right
                                           <$> searchPatronsNum conn n
                                         _ -> return $ Left
                                                "Patron Number is malformed"
               Just "Last Name"     -> Right <$> searchPatronsLastName conn sTerm
               Just "Email Address" -> case emailValid sTerm of
                                         Right e -> Right
                                           <$> searchPatronsEmail conn e
                                         _ -> return $ Left "Email is malformed"
               _ -> fail $ "Bug: Bad Search Type: " ++ show sType
             case sRes of
               Left err -> displayFailure resArea err
               Right ps -> displayPatronTable resArea ps conn infoFn extra

  let renderFld = toElement . fieldContent
  return
    [ renderFld searchTypeFld
    , renderFld searchFld
    , pad $
      split
      [ ( 9 , pad $ center #+! element resArea )
      , ( 3 , right #+! element searchBtn          )
      ]
    ]
  where
  searchRads = Radios "searchOptions" True $ map radOpt
    [ "Patron Number"
    , "Last Name"
    , "Email Address"
    ]

-- }}}

-- Display Patron Table {{{

displayPatronTable :: Element
                   -> [Patron]
                   -> Connection
                   -> PatronSearch extra
                   -> extra
                   -> IO ()
displayPatronTable e ps conn infoFn extra = void $ do
  tbl <- UI.table # set widthPerc "100" #+
           [ thead #+! tableHdr
           , tbody #+ map tableRow ps
           ]
  element e # set children [ tbl ]
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
      infoFn e conn p ps extra
    element a #~ s
  tableRow p = UI.tr #+
    [ td $ patLink p $        firstName   p
    , td $ patLink p $        lastName    p
    , td $ patLink p $        phoneNumber p
    , td $ patLink p $        emailAddr   p
    , td $ patLink p $ show $ patronNum   p
    ]

-- }}}

