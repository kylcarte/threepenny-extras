
module Utils where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

(#~) :: IO Element -> IO [Element] -> IO Element
e #~ m = do
  es <- m
  e # set children es

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m

