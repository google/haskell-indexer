{-# LANGUAGE CPP #-}
module CppInclude where

beforeInclude = 1

#include "DummyInclude.hs"

afterInclude = includedFun + beforeInclude
