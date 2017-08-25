{-# LANGUAGE ForeignFunctionInterface #-}
module ForeignImport where

foreign import ccall "pureFfiDep" pureFfiDep :: Int -> Int
