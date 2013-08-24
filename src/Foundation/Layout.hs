{-# LANGUAGE DeriveFunctor #-}

module Foundation.Layout where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Foundation.Common

import Data.Maybe (catMaybes, isJust)


-- Row {{{

data Row a = Row
  { rowCollapse :: Bool
  , rowColumns  :: [Column a]
  } deriving (Functor)

instance ToElements a => ToElement (Row a) where
  toElement (Row coll cs)
    | maybe True (<= 12) (widthSmall cs) && maybe True (<= 12) (widthLarge cs) =
      divClasses (if coll then ["row","collapse"] else ["row"]) #+
        map toElement cs
    | otherwise = fail $ "Column widths were greater than 12: (" ++ show (widthSmall cs) ++ ") (" ++ show (widthLarge cs) ++ ")"

widthSmall :: [Column a] -> Maybe Int
widthSmall cs = do
  ms <- mapM smallLayout cs
  let ns = map (\l -> [layoutWidth l,layoutOffset l]) ms
  return $ sum $ concat ns
  
widthLarge :: [Column a] -> Maybe Int
widthLarge cs = do
  ms <- mapM largeLayout cs
  let ns = map (\l -> [layoutWidth l,layoutOffset l]) ms
  return $ sum $ concat ns

collapseRow :: [Column a] -> Row a
collapseRow = Row True

paddedRow :: [Column a] -> Row a
paddedRow = Row False

-- }}}

-- Col {{{

data Column a = Column
  { smallLayout :: Maybe Layout
  , largeLayout :: Maybe Layout
  , colContent  :: a
  } deriving (Functor)

instance ToElements a => ToElement (Column a) where
  toElement (Column smL lgL a) = do
    smOpts <- formSmallOpts smL
    lgOpts <- formLargeOpts lgL
    let opts = catMaybes $ smOpts ++ lgOpts
    cts <- toElements a
    divClasses (opts ++ ["columns"]) #+ map element cts
    where
    formSmallOpts = formSizeOpts ("small-" ++)
    formLargeOpts = formSizeOpts ("large-" ++)
    formSizeOpts :: (String -> String) -> Maybe Layout -> IO [Maybe String]
    formSizeOpts _ Nothing = return []
    formSizeOpts size (Just l@(Layout cen off wid))
      | wid > 0 && off < 12 && (wid + off) <= 12
        = return
            [ maybeWhen cen       $ size "centered"
            , maybeWhen (off > 0) $ size $ "offset-" ++ show off
            , maybeWhen True      $ size $ show wid
            ]
      | otherwise = fail $ "Bad column layout: " ++ show l

stackAlways :: a -> Column a
stackAlways = Column Nothing Nothing

uniformLayout :: Layout -> a -> Column a
uniformLayout smL = Column (Just smL) Nothing

stackOnSmall :: Layout -> a -> Column a
stackOnSmall lgL = Column Nothing (Just lgL)

overrideSmall :: Layout -> Layout -> a -> Column a
overrideSmall smL lgL = Column (Just smL) (Just lgL)

-- Layout

data Layout = Layout
  { layoutCentered :: Bool
  , layoutOffset   :: Int
  , layoutWidth    :: Int
  } deriving Show

centered ::        Int -> Layout
centered = Layout True 0

offset   :: Int -> Int -> Layout
offset o = Layout False o

colWidth ::        Int -> Layout
colWidth = Layout False 0

-- }}}

-- Grid {{{

data Grid a = Grid
  { smallRowSize :: Maybe Int
  , largeRowSize :: Maybe Int
  , gridContents :: [a]
  } deriving (Functor)

instance ToElements a => ToElement (Grid a) where
  toElement (Grid smSize lgSize cs) = do
    opts <- mkOpts
    css  <- mapM toElements cs
    UI.ul # set classes (catMaybes opts) # set UI.style [] #+ map ((UI.li #+) . map element) css
    where
    mkOpts
      | isJust smSize || isJust lgSize = return
        [ fmap (\i -> "small-block-grid-" ++ show i) smSize
        , fmap (\i -> "large-block-grid-" ++ show i) lgSize
        ]
      | otherwise = fail "Grid must have at least a small or a large layout"

uniformGridSize :: Int -> [a] -> Grid a
uniformGridSize smSize = Grid (Just smSize) Nothing

stackGridOnSmall :: Int -> [a] -> Grid a
stackGridOnSmall lgSize = Grid Nothing (Just lgSize)

overrideGridSize :: Int -> Int -> [a] -> Grid a
overrideGridSize smSize lgSize = Grid (Just smSize) (Just lgSize)

-- }}}

