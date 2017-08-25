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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Indexer.Backend.Ghc.Test.TypeLinkTestBase (allTests) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Language.Haskell.Indexer.Backend.Ghc.Test.TestHelper

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure)

type AssertionInEnv = ReaderT TestEnv IO ()

testDataVarBinds :: AssertionInEnv
testDataVarBinds = assertXRefsFrom ["typelink/DataVarBinds.hs"] $
    -- Type variable.
    declAt (3,9) >>= singleUsage >>= includesPos (3,26)

testAliasVarBinds :: AssertionInEnv
testAliasVarBinds = assertXRefsFrom ["typelink/AliasVarBinds.hs"] $
    declAt (3,8) >>= singleUsage >>= includesPos (3,13)

testClassVarBinds :: AssertionInEnv
testClassVarBinds = assertXRefsFrom ["typelink/ClassVarBinds.hs"] $ do
    -- Class variable.
    declAt (3,9) >>= singleUsage >>= includesPos (4,10)
    -- Free variable in class method.
    declAt (4,15) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (4,15) u1
            includesPos (4,23) u2
        _ -> checking $ assertFailure "Usage count differs"

testForallBinds :: AssertionInEnv
testForallBinds = assertXRefsFrom ["typelink/ForallBinds.hs"] $ do
    -- For now implicit typevars get the declaration emitted at the first
    -- reference. Might change in future.
    declAt (4,13) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (4,13) u1
            includesPos (4,18) u2
        _ -> checking $ assertFailure "Usage count differs"
    -- Explicit typevars have the decl nicely on the foralled occurence.
    declAt (7,20) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (7,24) u1
            includesPos (7,29) u2
        _ -> checking $ assertFailure "Usage count differs"

testScopedTypeVarBinds :: AssertionInEnv
testScopedTypeVarBinds = assertXRefsFrom ["typelink/ScopedTypeVar.hs"] $ do
    declAt (4,13) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (4,13) u1
            includesPos (4,18) u2
        _ -> checking $
            assertFailure "Non-scoped top tyvar usage count differs"
    declAt (7,12) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (7,12) u1
            includesPos (7,17) u2
        _ -> checking $
            assertFailure "Non-scoped local tyvar usage count differs"
    declAt (10,18) >>= usages >>= \case
        [u1, u2, u3, u4] -> do
            includesPos (10,22) u1
            includesPos (10,27) u2
            includesPos (13,13) u3
            includesPos (13,18) u4
        _ -> checking $ assertFailure "Scoped usage count differs"

testInlineSignature :: AssertionInEnv
testInlineSignature = assertXRefsFrom ["typelink/InlineSig.hs"] $
    declAt (4,15) >>= usages >>= \case
        [sig, inlineSig] -> do
            includesPos (4,19) sig
            includesPos (5,20) inlineSig
        _ -> checking $ assertFailure "Usage count differs"

testLetVarBinds :: AssertionInEnv
testLetVarBinds = assertXRefsFrom ["typelink/LetSig.hs"] $
    declAt (4,14) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (4,14) u1
            includesPos (4,19) u2
        _ -> checking $ assertFailure "Usage count differs"

testSimpleSignature :: AssertionInEnv
testSimpleSignature = assertXRefsFrom ["typelink/SimpleSig.hs"] $ do
    -- This case is interesting, as the usage is at the top of the
    -- type tree (and is missed by using 'universeBi').
    declAt (4,13) >>= singleUsage >>= includesPos (4,13)
    -- This case just for completeness.
    declAt (7,18) >>= singleUsage >>= includesPos (7,22)

testDataContextRef :: AssertionInEnv
testDataContextRef = assertXRefsFrom ["typelink/DataContext.hs"] $
    declAt (5,18) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (5,10) u1
            includesPos (5,26) u2
        _ -> checking $ assertFailure "Usage count differs"

testContextBinds :: AssertionInEnv
testContextBinds = assertXRefsFrom ["typelink/ContextBinds.hs"] $ do
    -- In the unscoped case, we put the decl on the first occurence,
    -- including the context. It's somewhat arbitrary, but at least
    -- makes sure we don't miss the context.
    declAt (4,11) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (4,11) u1
            includesPos (4,16) u2
        _ -> checking $ assertFailure "Unscoped usage count differs"
    -- This is a fun case where a type var only appears in the context.
    declAt (7,12) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (7,12) u1
            includesPos (7,15) u2
        _ -> checking $ assertFailure "Context-only usage count differs"
    declAt (10,15) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (10,23) u1
            includesPos (10,29) u2
        _ -> checking $ assertFailure "Scoped usage count differs"


-- | Prepares the tests to run with the given test environment.
allTests :: TestEnv -> [Test]
allTests env =
    [ envTestCase "data-var-binds" testDataVarBinds
    , envTestCase "alias-var-binds" testAliasVarBinds
    , envTestCase "class-var-binds" testClassVarBinds
    , envTestCase "forall-binds" testForallBinds
    , envTestCase "scoped-var-binds" testScopedTypeVarBinds
    , envTestCase "inline-signature" testInlineSignature
    , envTestCase "let-var-binds" testLetVarBinds
    , envTestCase "simple-signature" testSimpleSignature
    , envTestCase "data-context-ref" testDataContextRef
    , envTestCase "context-binds" testContextBinds
    ]
  where
    envTestCase name test = testCase name (runReaderT test env)
