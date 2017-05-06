{-# LANGUAGE TemplateHaskell #-}
module UsesTH where

import Language.Haskell.TH

$(runQ [d|
    thFun a = 41
    thVar = 3
    |])

afterTH = thFun thVar
