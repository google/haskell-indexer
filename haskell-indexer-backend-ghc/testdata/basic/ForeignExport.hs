{-# LANGUAGE ForeignFunctionInterface #-}
module ForeignExport where

foreign export ccall exportedFun :: Int -> Int

exportedFun :: Int -> Int
exportedFun = (1+)
