
module Common where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Data.List (intersperse)

-- only child
(#+!) :: IO Element -> IO Element -> IO Element
p #+! c = p #+ [c]

infixl 8 #+!

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
link lab addr = UI.a # set UI.href addr # set UI.html lab

image :: String -> IO Element
image source = UI.img # set UI.src source

listItems :: [IO Element] -> [IO Element]
listItems = map $ \e -> UI.li #+ [e]

buttonLink :: String -> String -> IO Element
buttonLink lab addr = UI.li #+ [ link lab addr # set UI.class_ "button" ]

-- }}}

-- Elements {{{

nav :: IO Element
nav = mkElement "nav"

section :: IO Element
section = mkElement "section"

article :: IO Element
article = mkElement "article"

aside :: IO Element
aside = mkElement "aside"

footer :: IO Element
footer = mkElement "footer"

label :: IO Element
label = mkElement "label"

-- }}}

-- Attributes  {{{

role :: WriteAttr Element String
role = strAttr "role"

classes :: WriteAttr Element [String]
classes = mkWriteAttr (set' UI.class_ . unwords)

data_slug :: WriteAttr Element String
data_slug = strAttr "data-slug"

placeholder :: WriteAttr Element String
placeholder = strAttr "placeholder"

strAttr :: String -> WriteAttr Element String
strAttr = mkWriteAttr . set' . attr

-- }}}

