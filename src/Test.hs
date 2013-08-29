{-# LANGUAGE OverloadedStrings #-}

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Foundation.Common

import Control.Monad

main :: IO ()
main = do
  startGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Just "index.html"
    , tpStatic     = "static/"
    } setup

setup :: Window -> IO ()
setup w = void $
  getBody w #+ [ UI.div # set (strAttr "ng-app") "" #~ "I can add: 1 + 2 = {{ 1 + 2 }}" ]

