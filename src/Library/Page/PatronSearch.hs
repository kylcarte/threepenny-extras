
module Library.Page.PatronSearch where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (row)

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
     (Element,Element) -- drawArea, buttonArea
  -> Connection        -- DB Connection
  -> Patron            -- this Patron
  -> [Patron]          -- search results (for "Back", etc.)
  -> extra             -- whatever else is needed
  -> IO ()

-- Patron Search {{{

type Search search extra = 
     (Element,Element) -- drawArea, buttonArea
  -> Connection        -- DB Connection
  -> [search]          -- search results (for "Back", etc.)
  -> extra             -- whatever else is needed
  -> IO ()

search :: [(String,String -> IO (Either String [s]))]
       -> String
       -> Connection
       -> Search s extra
       -> extra
       -> Page
search searchOpts pageNm conn searchFn extra = do
  searchTypeFld <- optional (radsField searchRads) "Search By"
  searchFld     <- optional textField ""
  resArea       <- UI.div
  btnArea       <- UI.div
  let resetFlds = do setValue searchTypeFld (radioDefault searchRads)
                     setValue searchFld     ""
  clearBtn      <- toElement $
    Button (LabelStr "Clear") (secondaryBtn radiusBtnStyle) $ const $ void $
      do resetFlds
         element resArea # set children []
  searchBtn     <- toElement $
    Button (LabelStr "Search") radiusBtnStyle $ const $
      do sTerm <- getValue searchFld
         if null sTerm
           then displayFailure resArea "You must enter a search term"
           else do
             msType <- getValue searchTypeFld
             case msType of
               Nothing -> displayFailure resArea "Select a search type"
               Just sType -> do
                 resetFlds
                 let go [] = fail $ "Bug: Bad Search Type: " ++ show sType
                     go ((typ,act):rest) = if typ /= sType
                       then go rest
                       else act sTerm
                 sRes <- go searchOpts
                 case sRes of
                   Left err -> displayFailure resArea err
                   Right rs -> searchFn (resArea,btnArea) conn rs extra

  let renderFld = toElement . fieldContent
  return
    [ renderFld searchTypeFld
    , renderFld searchFld
    , pad $
      split
      [ ( 9 , pad $ center #+! element resArea )
      , ( 3 , right #+
                [ element searchBtn
                , UI.br
                , element clearBtn
                , UI.br
                , element btnArea
                ])
      ]
    ]
  where
  searchRads = Radios
    (pageNm ++ "searchOptions")
    (Just $ fst $ head searchOpts)
    (map (radOpt . fst) searchOpts)

patronSearch' :: String -> Connection -> PatronSearch () -> Page
patronSearch' pg conn act = patronSearch pg conn act ()

patronSearch :: String -> Connection -> PatronSearch extra -> extra -> Page
patronSearch pageNm conn infoFn extra = do
  searchTypeFld <- optional (radsField searchRads) "Search By"
  searchFld     <- optional textField ""
  resArea       <- UI.div
  btnArea       <- UI.div
  let resetFlds = do setValue searchTypeFld (radioDefault searchRads)
                     setValue searchFld     ""
  clearBtn      <- toElement $
    Button (LabelStr "Clear") (secondaryBtn radiusBtnStyle) $ const $ void $
      do resetFlds
         element resArea # set children []
  searchBtn     <- toElement $
    Button (LabelStr "Search") radiusBtnStyle $ const $
      do sTerm <- getValue searchFld
         if null sTerm
           then displayFailure resArea "You must enter a search term"
           else do
             sType <- getValue searchTypeFld
             resetFlds
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
               Right ps -> displayPatronTable (resArea,btnArea) ps conn infoFn extra

  let renderFld = toElement . fieldContent
  return
    [ renderFld searchTypeFld
    , renderFld searchFld
    , pad $
      split
      [ ( 9 , pad $ center #+! element resArea )
      , ( 3 , right #+
                [ element searchBtn
                , UI.br
                , element clearBtn
                , UI.br
                , element btnArea
                ])
      ]
    ]
  where
  searchRads = Radios (pageNm ++ "searchOptions") (Just "Patron Number") $ map radOpt
    [ "Patron Number"
    , "Last Name"
    , "Email Address"
    ]

-- }}}

-- Display Patron Table {{{

displayPatronTable' ::
     (Element,Element)
  -> [Patron]
  -> Connection
  -> PatronSearch ()
  -> IO ()
displayPatronTable' es ps conn act =
  displayPatronTable es ps conn act ()

displayPatronTable :: (Element,Element)
                   -> [Patron]
                   -> Connection
                   -> PatronSearch extra
                   -> extra
                   -> IO ()
displayPatronTable (drawArea,btnArea) ps conn infoFn extra = void $ do
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

