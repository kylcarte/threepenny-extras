
module Input where

import Utils

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Applicative
import Control.Monad
import Data.Maybe (catMaybes)

-- Buttons {{{

mkButton :: String -> IO (Element, Element)
mkButton ttl = do
  button <- UI.button #+ [string ttl]
  view   <- UI.p #+ [element button]
  return (button, view)

-- }}}

-- Select {{{

{-
mkSelect :: [(String,a)] -> (a -> IO void) -> IO Element
mkSelect opts behav = do
  sel <- mkDropDown $ map fst opts
  let getVal = do
        i <- get UI.selection sel
        return $ (snd . (opts !!)) <$> i
  onSelect sel getVal behav
  return sel
-}

onSelect :: Element -> (String -> IO void) -> IO ()
onSelect sel behav = on (domEvent "change") sel $ \_ -> do
  v <- get value sel
  behav v

mkSelect :: [(String,String)] -> IO Element
mkSelect opts = UI.select #+ (mkOpt ("--","-1") : map mkOpt opts)
  where
  mkOpt (opt,val) = UI.option # set html opt # set value val

-- }}}

-- Radio Buttons {{{

mkRadios :: String -> [(String,String)]
         -> IO ([Element],[Element],IO (Maybe String))
         -- Given a mapping from labels to values,
         --  Returns a list of radio button elements,
         --  a list of radio/label <span>'s,
         --  and a method for getting the selected value, if there is one.
mkRadios nm opts = do
  (rads,views) <- fmap unzip $ forM opts $ \(opt,val) -> do
                    rad <- mkRadio nm val
                    view <- UI.span #+ [ element rad , string opt ]
                    return (rad,view)
  let getVal = do
        vs <- fmap catMaybes $ forM rads $ \r -> do
          chkd <- get UI.checked r
          if chkd
          then Just <$> get value r
          else return Nothing
        case vs of
          [v] -> return $ Just v
          []  -> return Nothing
          _   -> fail $ "More than one radio button checked??: " ++ show vs
  return (rads,views,getVal)

-- Make a single radio button with a name and a value.
--  NB: the name is important for radio buttons, since they are grouped by name.
mkRadio :: String -> String -> IO Element
mkRadio nm val = UI.input # set UI.type_ "radio" # set UI.name nm # set value val

-- }}}

-- Checkboxes {{{

mkCheckboxes :: [(String,String)]
             -> IO ([Element],[Element],IO [String])
             -- Given a mapping from labels to values,
             --  Returns list of checkboxes, a list of checkbox/label <span>'s,
             --  and a method for getting the list of values of all the checked boxes.
mkCheckboxes opts = do
  (chks,views) <- fmap unzip $ forM opts $ \(opt,val) -> do
                    chk <- mkCheckbox val
                    view <- UI.span #+ [ element chk , string opt ]
                    return (chk,view)
  let getVal = fmap catMaybes $ forM chks $ \c -> do
                 chkd <- get UI.checked c
                 if chkd
                 then Just <$> get value c
                 else return Nothing
  return (chks,views,getVal)

-- Make a single checkbox with a value
mkCheckbox :: String -> IO Element
mkCheckbox val = UI.input # set UI.type_ "checkbox" # set value val

-- }}}

