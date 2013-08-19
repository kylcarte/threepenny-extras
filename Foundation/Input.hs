
module Foundation.Input where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Applicative
import Control.Monad

import Common

-- Dropdown {{{

data Dropdown = Dropdown
  { dropdownId    :: String
  , dropdownLabel :: Label
  , dropdownOpts  :: [Option]
  }

instance ToElement Dropdown where
  toElement d = UI.form # set UI.class_ "custom" #+
    [ label # set for (dropdownId d) # labelElt (dropdownLabel d)
    , UI.select # set UI.id_ (dropdownId d) #+ map toElement (dropdownOpts d)
    ]

-- Option

data Option = Option
  { optString   :: String
  , optDisabled :: Bool
  }

instance ToElement Option where
  toElement o = option # set disabled (optDisabled o) #~ optString o

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

