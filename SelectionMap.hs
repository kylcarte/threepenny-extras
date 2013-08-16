
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Applicative
import Control.Monad
import qualified Data.Map as M

main :: IO ()
main = do
  startGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = ""
    } setup

setup :: Window -> IO ()
setup w = void $ do
  return w # set title "Page Switching"
  div <- UI.div #~ emptyPage
  -- a persistent list
  pList <- UI.ul

  sel1@(sel,_) <- mkSelectionMap
                    [ ( "--"     , emptyPage   )
                    , ( "Page 1" , page1       )
                    , ( "Page 2" , page2 pList )
                    ]

  onSelect sel1 (element div #~)

  getBody w #+
    [ row [ string "Select a Page: " , element sel ]
    , element div
    ]

-- Pages {{{

emptyPage :: Page
emptyPage = page []

page1 :: Page
page1 = do
  list <- UI.ul
  (but1,but1view) <- mkButton "Here I am!"
  on UI.click but1 $ \_ -> do
    element list #+ [ UI.li # set html "You clicked me!" ]
  page
    [ UI.h1 #+ [ string "This is Page 1." ]
    , UI.h2 #+ [ string "It has a button." ]
    , UI.h2 #+ [ string "Notice that the click messages aren't persistent" ]
    , element but1view
    , element list
    ]

page2 :: Element -> Page
page2 list = do
  (but1, but1view) <- mkButton "I will crush you!"
  on UI.click but1 $ \_ -> do
    element list #+ [ UI.li # set html "Argh not again!" ]
  page
    [ UI.h1 #+ [ string "This is Page 2." ]
    , UI.h2 #+ [ string "It also has a button." ]
    , UI.h2 #+ [ string "Its messages are, in fact, persistent." ]
    , element but1view
    , element list
    ]

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

