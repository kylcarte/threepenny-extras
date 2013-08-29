{-# LANGUAGE TemplateHaskell #-}

module Library.DB.TH where

import Language.Haskell.TH

mkTable :: Name -> Q [Dec]
mkTable t = do


