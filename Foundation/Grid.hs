
module Foundation.Grid where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Applicative
import Control.Monad
import Data.Maybe (catMaybes, isJust)
import Data.Monoid 

import Common

data Layout = Layout
  { layoutCentered :: Bool
  , layoutOffset   :: Int
  , layoutWidth    :: Int
  } deriving Show

centered ::        Int -> Layout
centered = Layout True 0

offset   :: Int -> Int -> Layout
offset o = Layout False o

data Column = Column
  { smallLayout :: Layout
  , largeLayout :: Layout
  , colContent     :: [IO Element]
  }

row :: [Column] -> IO Element
row cs = divClass "row" #+ map mkCol cs
  where
  mkCol c  = do opts <- mkOpts c
                divClasses (catMaybes opts ++ ["columns"]) #+ colContent c
  mkOpts c = do s <- mkSmallOpts (smallLayout c)
                l <- mkLargeOpts (largeLayout c)
                return (s ++ l)

mkSmallOpts :: Layout -> IO [Maybe String]
mkSmallOpts = mkSizeOpts ("small-" ++)

mkLargeOpts :: Layout -> IO [Maybe String]
mkLargeOpts = mkSizeOpts ("large-" ++)

mkSizeOpts :: (String -> String) -> Layout -> IO [Maybe String]
mkSizeOpts size l@(Layout c off w)
  | w > 0 && off < 12
    = return
        [ maybeWhen c         $ size "centered"
        , maybeWhen (off > 0) $ size $ show off
        , maybeWhen True      $ size $ show w
        ]
  | otherwise = fail $ "Bad column layout: " ++ show l

maybeWhen :: Bool -> a -> Maybe a
maybeWhen b a = if b then Just a else Nothing



data Grid = Grid
  { smallRowSize :: Maybe Int
  , largeRowSize :: Maybe Int
  , gridContents :: [[IO Element]]
  }

blockGrid :: Grid -> IO Element
blockGrid g = do
  opts <- mkOpts g
  UI.ul # set classes (catMaybes opts) #+ map (UI.li #+) (gridContents g)
  where
  mkOpts g
    | isJust (smallRowSize g) || isJust (largeRowSize g) = return
      [ fmap (\i -> "small-block-grid-" ++ show i) $ smallRowSize g
      , fmap (\i -> "large-block-grid-" ++ show i) $ largeRowSize g
      ]
    | otherwise = fail "Grid must have at least a small or a large layout"

