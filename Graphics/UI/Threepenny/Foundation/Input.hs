{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.UI.Threepenny.Foundation.Input where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Applicative
import Control.Monad

import Common

data Button = Button
  { buttonLabel  :: Label
  , buttonAction :: IO ()
  }

instance ToElement Button where
  toElement (Button lbl act) = do
    but <- UI.button # set classes ["button"] #+ toElements lbl
    on UI.click but $ const act
    element but

-- Dropdown {{{

data Dropdown = Dropdown
  { dropdownId       :: String
  , dropdownLabel    :: Label
  , dropdownBlankDef :: Bool
  , dropdownOpts     :: [Option]
  }

instance ToElementAction Dropdown String where
  toElementAction d = do
    dd <- UI.form # set UI.class_ "custom" #+
      [ label # set for (dropdownId d) # labelElt (dropdownLabel d)
      , UI.select # set UI.id_ (dropdownId d) #+ map toElement
        ((if dropdownBlankDef d then (opt "--" :) else id) $
         dropdownOpts d)
      ]
    return (dd, get value dd)

-- Option

data Option = Option
  { optString   :: String
  , optDisabled :: Bool
  }

instance ToElement Option where
  toElement o = option #
    set disabled (optDisabled o) #
    set value (optString o) #~
      optString o

opt :: String -> Option
opt s = Option s False

disableOption :: Option -> Option
disableOption o = o { optDisabled = True }

enableOption :: Option -> Option
enableOption o = o { optDisabled = False }

-- }}}

-- LegendFor {{{

data LegendFor a = LegendFor
  { legendStr :: String
  , legendFor :: a
  }

instance ToElements a => ToElement (LegendFor a) where
  toElement (LegendFor leg a) =
    fieldset #+
      ( legend #~ leg
      : toElements a
      )

-- }}}

-- LabelFor {{{

data LabelFor a = LabelFor
  { labelId     :: String
  , labelLab    :: Label
  , labelFor    :: a
  }

instance ToElement a => ToElements (LabelFor a) where
  toElements (LabelFor idStr lbl a) =
    [ label # set for idStr #+ toElements lbl
    , toElement a # set UI.id_ idStr
    ]

-- }}}

