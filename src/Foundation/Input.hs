{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Foundation.Input where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Foundation.Common

import Control.Applicative
import Control.Monad
import Data.Maybe (catMaybes)

-- ButtonGroup {{{

data ButtonGroup = ButtonGroup
  { buttonGroupStyle :: ButtonStyle
  , buttonGroup      :: [Button]
  }

instance ToElement ButtonGroup where
  toElement (ButtonGroup s g) 
    | length g > 8 = fail "Button Group has too many buttons! (css can't handle more than 8, sorry)"
    | otherwise = UI.ul #
      set classes (bgLen : "button-group" : buttonClasses s) #+
        map buildBtn g
    where
    buildBtn b = UI.li #+ [toElement $ applyButtonGroupRadius b]
    bgLen = "even-" ++ show (length g)
    applyButtonGroupRadius :: Button -> Button
    applyButtonGroupRadius b = b
      { buttonStyle = ButtonStyle
                        (buttonSize  $ buttonStyle b)
                        (buttonColor $ buttonStyle b)
                        Nothing
                        (buttonDisabled $ buttonStyle b)
      }

-- }}}

-- Button {{{

data Button = Button
  { buttonLabel  :: Label
  , buttonStyle  :: ButtonStyle
  , buttonAction :: Element -> IO ()
  }

instance ToElement Button where
  toElement (Button lbl s act) = do
    lbl' <- toElements lbl
    but <- UI.button # set classes ("button" : buttonClasses s) #+ map element lbl'
    on UI.click but $ const $ act but
    element but

plainBtnStyle :: ButtonStyle
plainBtnStyle = ButtonStyle Nothing Nothing Nothing False

roundBtnStyle :: ButtonStyle
roundBtnStyle = ButtonStyle Nothing Nothing (Just BtnRound) False

radiusBtnStyle :: ButtonStyle
radiusBtnStyle = ButtonStyle Nothing Nothing (Just BtnRadius) False

buttonClasses :: ButtonStyle -> [String]
buttonClasses s = catMaybes
  [ bSize <$> buttonSize s
  , bColor <$> buttonColor s
  , bRadius <$> buttonRadius s
  , maybeWhen (buttonDisabled s) "disabled"
  ]
  where
  bSize sz = case sz of
    BtnTiny  -> "tiny"
    BtnSmall -> "small"
    BtnLarge -> "large"
  bColor co = case co of
    BtnSecondary -> "secondary"
    BtnAlert     -> "alert"
    BtnSuccess   -> "success"
  bRadius rd = case rd of
    BtnRadius -> "radius"
    BtnRound  -> "round"

data ButtonStyle = ButtonStyle
  { buttonSize     :: Maybe ButtonSize
  , buttonColor    :: Maybe ButtonColor
  , buttonRadius   :: Maybe ButtonRadius
  , buttonDisabled :: Bool
  }

alertBtn :: ButtonStyle -> ButtonStyle
alertBtn bs = bs { buttonColor = Just BtnAlert }

successBtn :: ButtonStyle -> ButtonStyle
successBtn bs = bs { buttonColor = Just BtnSuccess }

secondaryBtn :: ButtonStyle -> ButtonStyle
secondaryBtn bs = bs { buttonColor = Just BtnSecondary }

data ButtonSize
  = BtnTiny
  | BtnSmall
  | BtnLarge

data ButtonColor
  = BtnSecondary
  | BtnAlert
  | BtnSuccess

data ButtonRadius
  = BtnRadius
  | BtnRound

-- }}}

-- Dropdown {{{

data Dropdown = Dropdown
  { dropdownId       :: String
  , dropdownBlankDef :: Bool
  , dropdownOpts     :: [Option]
  }

instance ToElementAction Dropdown String where
  toElementAction d = do
    sel <- UI.select # set UI.id_ (dropdownId d) #+ map toElement
             ((if dropdownBlankDef d then (opt "" :) else id) $
              dropdownOpts d)
    let getVal = get value sel
    let setVal s = void $ element sel # set value s
    {-void $ element sel #
                     set UI.selection
                       ((+ 1) <$> elemIndex s (map optString $ dropdownOpts d))
                       -}
    return (sel,getVal,setVal)

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

-- Radios {{{

data Radios a = Radios
  { radioName       :: String
  , radioDefault    :: Maybe String
  , radioOptions    :: [(a,String)]
  }

radOpt :: String -> (IO Element, String)
radOpt s = (string s, s)

instance ToElements a => ToElementsAction (Radios a) (Maybe String) where
  toElementsAction (Radios _ _ []) = fail "Empty Radio Options"
  toElementsAction (Radios nm mdef (r:rs)) = do
    (r',v)   <- mkOption (r,0)
    (rs',vs) <- fmap unzip $ mapM mkOption $ zip rs [1..]
    let setVal ms = forM_ (r':rs') $ \rad -> do
                      mv <- Just <$> get value rad
                      void $ element rad # set UI.checked (mv == ms)
    let getVal = do vals <- fmap catMaybes $ forM (r':rs') $ \rad -> do
                      chkd <- get UI.checked rad
                      if chkd
                        then Just <$> get value rad
                        else return Nothing
                    case vals of
                      [x] -> return $ Just x
                      []  -> return Nothing
                      _   -> fail $ "More than one radio button checked??: " ++ show vals
    return (v:vs,getVal,setVal)
    where
    mkOption :: ToElements a => ((a,String),Integer) -> IO (Element,Element)
    mkOption ((a,val),i) = do
      let idStr = nm ++ show i
      inp <- UI.input #*
               [ set UI.name nm
               , set UI.type_ "radio"
               , set UI.id_ idStr
               , set value val
               , set checked (mdef == Just val)
               ]
      cts <- toElements a
      lab <- label #
             set for idStr #
             set UI.valign "middle" #+
               ( element inp
               : UI.span #~ " "
               : map element cts
               )
      return (inp,lab)

-- }}}

-- Checkboxes {{{

data Checkboxes a = Checkboxes
  { checkboxName     :: String
  , checkboxDefaults :: [String]
  , checkboxOptions  :: [(a,String)]
  }

instance ToElements a => ToElementsAction (Checkboxes a) [String] where
  toElementsAction (Checkboxes _ _ []) = fail "Empty Checkbox Options"
  toElementsAction (Checkboxes nm defs (c:cs)) = do
    (c',v)   <- mkOption c
    (cs',vs) <- fmap unzip $ mapM mkOption cs
    let setVal ss = forM_ (c':cs') $ \cbx -> do
                      mv <- get value cbx
                      when (mv `elem` ss) $ void $
                        element cbx # set UI.checked True
    let getVal = fmap catMaybes $ forM (c':cs') $ \cbx -> do
                   chkd <- get UI.checked cbx
                   if chkd
                     then Just <$> get value cbx
                     else return Nothing
    return (v:vs,getVal,setVal)
    where
    mkOption (a,val) = do
      let chkd = val `elem` defs
      inp <- UI.input #*
               [ set UI.name nm
               , set UI.type_ "checkbox"
               , set UI.id_ nm
               , set UI.style [("display","none")]
               , set value val
               , if chkd then set UI.checked True else id
               ]
      cts <- toElements a
      lab <- label # set for nm #+
               ( element inp
               : UI.span #
                 set classes ((if chkd then ("checked" :) else id) ["custom","checkbox"])
               : map element cts
               )
      return (inp,lab)

-- }}}

-- LegendFor {{{

data LegendFor a = LegendFor
  { legendStr :: String
  , legendFor :: a
  }

instance ToElements a => ToElement (LegendFor a) where
  toElement (LegendFor leg a) = do
    cts <- toElements a
    fieldset #+
      ( legend #~ leg
      : map element cts
      )

-- }}}

-- LabelFor {{{

data LabelFor a = LabelFor
  { labelId     :: String
  , labelLab    :: Label
  , labelFor    :: a
  }

instance ToElement a => ToElements (LabelFor a) where
  toElements (LabelFor idStr lbl a) = do
    lbl' <- toElements lbl
    lab <- label # set for idStr #+ map element lbl'
    anch <- toElement a # set UI.id_ idStr
    return [ lab , anch ]

-- }}}

