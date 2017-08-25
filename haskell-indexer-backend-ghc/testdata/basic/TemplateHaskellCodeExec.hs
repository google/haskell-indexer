{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellCodeExec where

import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH
import UsedByTH (pureDep)

foo = $([|$(lift . pureDep $ 3)|])
