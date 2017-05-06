-- Copyright 2017 Google Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators #-}

-- | Landmine-proof GHC AST traversal.
module Language.Haskell.Indexer.Backend.GhcLens
    ( careful
    , carefulUniverse
    -- * Uniplate-like API (see details).
    , universe, children
    , universeBi
    ) where

import Control.Applicative
import Control.Exception
import Data.Data
import Data.Hashable
import qualified Data.HashSet as HS
import Data.IORef
import System.IO.Unsafe
import System.Mem.StableName

import Panic

data SeenNode where
  SeenNode :: StableName a -> SeenNode

instance Eq SeenNode where
  SeenNode sn == SeenNode sn' = eqStableName sn sn'

instance Hashable SeenNode where
  hashWithSalt salt (SeenNode sn) = hashWithSalt salt sn

type SeenNodes = IORef (HS.HashSet SeenNode)

{-|
A function that traverses a structure of type @s@ and finds values of
type @a@, applying a function @f@ to lift them into a `Monoid` @m@.

Does not recurse further into values of type @a@. If it detects a node
it has already traversed, it is skipped using the given Monoid's 'mempty'
in order to prevent traversing self-referential structures forever.

Nodes that would immediately throw a GHC 'Panic' are also skipped, but other
GHC exceptions are re-thrown.
-}
careful :: (Monoid m, Typeable a, Data s) => (a -> m) -> s -> m
careful f x = unsafeDupablePerformIO $ do
  seenNodes <- newIORef HS.empty
  carefulChildren seenNodes f x

-- | Returns all subelements of the same type (including self).
carefulUniverse :: (Typeable a, Data a) => a -> [a]
carefulUniverse a = a:careful carefulUniverse a

carefulChildren :: forall m a s. (Monoid m, Typeable a, Data s)
  => SeenNodes -> (a -> m) -> s -> IO m
carefulChildren nodesRef f
  = gmapQr (liftA2 mappend) (pure mempty) (careful' nodesRef f)

careful' :: forall m a s. (Monoid m, Typeable a, Data s)
  => SeenNodes -> (a -> m) -> s -> IO m
careful' nodesRef f node
  = handleGhcException handlePanic $ do
    _ <- evaluate node -- check for landmines before we do anything
    nodeName <- SeenNode <$> makeStableName node
    seenNodes <- readIORef nodesRef
    if HS.member nodeName seenNodes
      then return mempty
      else do
        writeIORef nodesRef $! HS.insert nodeName seenNodes
        case eqT of
          Just (Refl :: s :~: a) -> return (f node)
          Nothing -> carefulChildren nodesRef f node
  where
    handlePanic (Panic _) = return mempty
    handlePanic e = throwGhcExceptionIO e

-- | All values matching the type, deeply from the structure (including root).
universe :: (Typeable a, Data a) => a -> [a]
universe = carefulUniverse

-- | All values of type 'a' below the root of type 's'.
--
-- Warning! Edge case behavior differs from uniplate - if 'a'~'s', uniplate
-- would return only the root, while this returns everything except the root.
-- Such usecase is not intended for universeBi anyway - use universe or
-- children.
universeBi :: forall s a. (Typeable a, Data a, Data s) => s -> [a]
universeBi = concatMap carefulUniverse . (careful return :: s -> [a])

-- | Nearest descendants, excluding root.
children :: (Typeable a, Data a) => a -> [a]
children = careful return

