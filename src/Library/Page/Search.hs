
module Library.Page.Search where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (row)

import Foundation.Common
import Foundation.Input
import Foundation.Layout
import Foundation.Sections (Page)

import Library.Page.PatronInfo

import Database.SQLite.Simple

import Control.Monad

type Search search extra = 
     (Element,Element) -- drawArea, buttonArea
  -> Connection        -- DB Connection
  -> [search]          -- search results (for "Back", etc.)
  -> extra             -- whatever else is needed
  -> IO ()

search :: [(String,String -> IO (Either String [s]))] -- Search Options/Actions
       -> String                                      -- Page Name
       -> Connection                                  -- DB Connection
       -> Search s extra                              -- Search Results Action
       -> extra                                       --   and extra info
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

