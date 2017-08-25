{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellCodeExecFFI where

import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH
import ForeignImport (pureFfiDep)

foo = $([|$(lift . pureFfiDep $ 4)|])
