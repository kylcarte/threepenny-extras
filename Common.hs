{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Text.JSON
import Control.Monad
import Control.Monad.IO.Class

-- only child
(#+!) :: IO Element -> IO Element -> IO Element
p #+! c = p #+ [c]

infixl 8 #+!

-- Set contents to be raw html
(#~) :: IO Element -> String -> IO Element
e #~ s = e # set UI.html s

infixl 8 #~

-- apply all transformations
(#*) :: a -> [a -> a] -> a
(#*) = foldl (#)

infixl 8 #*


modify :: MonadIO m => Attr x a -> (a -> a) -> m x -> m x
modify at f mx = do
  x <- mx
  a <- liftIO $ get at x
  mx # set at (f a)

foundationGUI :: Config -> (Window -> IO ()) -> IO ()
foundationGUI c f = startGUI c $ \w -> void $ do
  UI.addStyleSheet w "normalize.css"
  UI.addStyleSheet w "foundation.css"
  getHead w #+
    [ script # set UI.src "/static/js/custom.modernizr.js"
    ]
  f w
  getBody w #+
    [ script # set UI.src "/static/js/foundation.min.js"
    , script #~ "$(document).foundation();"
    , script # set UI.src "/static/js/zepto.js"
    , script #~ "document.write('<script src=js/' + ('__proto__' in {} ? 'zepto' : 'jquery') + '.js><\\/script>')"
    ]

-- Label {{{

data Label
  = LabelStr String
  | LabelEls [IO Element]

instance ToElements Label where
  toElements (LabelStr s ) = [UI.span #~ s]
  toElements (LabelEls es) = es

labelElt :: Label -> IO Element -> IO Element
labelElt (LabelStr s ) e = e #~ s
labelElt (LabelEls es) e = e #+ es

-- }}}

-- Link {{{

data Link = Link
  { linkLabel :: Label
  , linkAddr  :: String
  }

instance ToElement Link where
  toElement el = lnk # labelElt (linkLabel el)
    where
    lnk = UI.a # set UI.href (linkAddr el)

-- }}}

-- ToElementAction {{{

class ToElementAction a act where
  toElementAction :: a -> IO (Element,IO act)

-- }}}

-- ToElement {{{

class ToElement a where
  toElement :: a -> IO Element

instance ToElement Element where
  toElement = return

instance ToElement (IO Element) where
  toElement = id

instance (ToElement a,ToElement b) => ToElement (Either a b) where
  toElement (Left a)  = toElement a
  toElement (Right b) = toElement b

-- }}}

-- ToElements {{{

class ToElements a where
  toElements :: a -> [IO Element]

instance ToElements (IO Element) where
  toElements = (:[])

instance ToElement a => ToElements [a] where
  toElements = map toElement

instance (ToElement a,ToElement b) => ToElements (a,b) where
  toElements (a,b) = [toElement a,toElement b]

instance (ToElement a,ToElement b,ToElement c) => ToElements (a,b,c) where
  toElements (a,b,c) = [toElement a,toElement b,toElement c]

instance (ToElement a,ToElement b,ToElement c,ToElement d)
  => ToElements (a,b,c,d) where
  toElements (a,b,c,d) = [toElement a,toElement b,toElement c,toElement d]

-- }}}

-- Div types {{{

rowClass :: IO Element
rowClass = divClass "row"

divClass :: String -> IO Element
divClass c = divClasses [c]

divClasses :: [String] -> IO Element
divClasses cs = UI.div # set classes cs

-- }}}

-- Convenience {{{

par :: String -> IO Element
par s = UI.p # set UI.html s

link :: String -> String -> IO Element
link lab addr = UI.a #*
  [ set UI.href addr
  , set UI.html lab
  ]

image :: String -> IO Element
image source = UI.img # set UI.src source

listItems :: [IO Element] -> [IO Element]
listItems = map $ \e -> UI.li #+ [e]

buttonLink :: String -> String -> IO Element
buttonLink lab addr = UI.li #+ [ link lab addr # set UI.class_ "button" ]

-- }}}

-- Elements {{{

divider :: IO Element
divider = UI.li # set classes ["divider"]

nav :: IO Element
nav = mkElement "nav"

section :: IO Element
section = mkElement "section"

script :: IO Element
script = mkElement "script"

article :: IO Element
article = mkElement "article"

aside :: IO Element
aside = mkElement "aside"

footer :: IO Element
footer = mkElement "footer"

label :: IO Element
label = mkElement "label"

option :: IO Element
option = mkElement "option"

fieldset :: IO Element
fieldset = mkElement "fieldset"

legend :: IO Element
legend = mkElement "legend"

-- }}}

-- Attributes  {{{

disabled :: WriteAttr Element Bool
disabled = mkWriteAttr $ \b ->
  if b
  then set' (strAttr "DISABLED") ""
  else const $ return ()

for :: WriteAttr Element String
for = strAttr "for"

role :: WriteAttr Element String
role = strAttr "role"

classes :: Attr Element [String]
classes = fromProp "class" fromJSON toJSON
  where
  toJSON = showJSON . unwords
  fromJSON js = case readJSON js of
    Ok a      -> words a
    Error err -> error err

data_ :: String -> WriteAttr Element String
data_ s = strAttr $ "data-" ++ s

placeholder :: WriteAttr Element String
placeholder = strAttr "placeholder"

strAttr :: String -> WriteAttr Element String
strAttr = mkWriteAttr . set' . attr

-- }}}

-- Helpers {{{

maybeWhen :: Bool -> a -> Maybe a
maybeWhen b a = if b then Just a else Nothing

-- }}}

