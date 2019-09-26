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
module Language.Haskell.Indexer.Backend.Ghc.Test.BasicTestBase (allTests) where

import Control.Monad ((>=>), unless, void)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Language.Haskell.Indexer.Backend.Ghc.Test.TestHelper

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure)

type AssertionInEnv = ReaderT TestEnv IO ()

-- | Tests that arguments of top-level functions, and their references, are
-- extracted. The argument references happen in various common syntactic
-- environments, such as if-then-else, case, etc.
testArgRef :: AssertionInEnv
testArgRef = assertXRefsFrom ["basic/ArgRef.hs"] $ do
    declAt (3,3) >>= singleUsage >>= includesPos (3,7)
    declAt (5,3) >>= singleUsage >>= includesPos (5,7)
    declAt (7,3) >>= singleUsage >>= includesPos (7,22)
    declAt (7,5) >>= singleUsage >>= includesPos (7,29)

-- | Tests recursive references in various contexts.
testRecursiveRef :: AssertionInEnv
testRecursiveRef = assertXRefsFrom ["basic/RecursiveRef.hs"] $ do
    -- Recursive function call without type signature targets the
    -- monomorphic binding. Test that we detect this.
    declAt (4,1) >>= singleUsage >>= includesPos (4,14)
    declAt (8,5) >>= usages >>= \case
        [u1, u2] -> do
            includesPos (6,9) u1
            includesPos (8,23) u2
        _ -> checking $ assertFailure "Usage count differs"
    -- Recursive fun with type signature should target using the
    -- polymorphic binding.
    declAt (11,1) >>= usages >>= \case
        [u1, u2] -> do
            -- Note: first one is the ref from the type signature (see #25).
            includesPos (10,1) u1
            includesPos (11,16) u2
        us -> checking $ assertFailure $ "Usage count differs " ++ show us
    -- Other interesting cases.
    declAt (13,1) >>= singleUsage >>= includesPos (14,16)
    declAt (14,1) >>= singleUsage >>= includesPos (13,16)
    declAt (16,1) >>= singleUsage >>= includesPos (16,12)

testDataDecl :: AssertionInEnv
testDataDecl = assertXRefsFrom ["basic/DataDecl.hs"] $ do
    -- Plain data type.
    -- Ctor references.
    declAt (3,11) >>= singleUsage >>= includesPos (6,3)
    declAt (3,15) >>= singleUsage >>= includesPos (7,4)
    -- Record constructor reference.
    -- Based on the AST there would be a reference from the compiler-generated
    -- implementation of the field accessor, but we filter the body of such
    -- generated functions (see GhcAnalyser.hs for reasons).
    declAt (9,15) >>= singleUsage >>= includesPos (12,4)
    -- Accessor function reference.
    declAt (9,24) >>= singleUsage >>= includesPos (14,5)
    -- TODO(robinpalotai): datatype ref (once supported).
    -- TODO(robinpalotai): ctor arg references (once supported).

-- | Test that function applications are emitted as calls.
testFunCall :: AssertionInEnv
testFunCall = assertXRefsFrom ["basic/FunCall.hs"] $ do
    declAt (3,1) >>= singleUsage >>= assertAll
        [ refKindIs Call
        , includesPos (4,11)
        , refContextIs `funk` declAt (4,1)
        ]
    declAt (6,1) >>= singleUsage >>= assertAll
        [ refKindIs Ref
        , includesPos (7,13)
        , refContextIs `funk` declAt (7,1)
        ]
    declAt (9,1) >>= usages >>= \case
        [u1, u2] -> do
            flip assertAll u1
                [ includesPos (10,10)
                , refKindIs Call
                ]
            flip assertAll u2
                [ includesPos (10,12)
                , refKindIs Ref
                ]
        _ -> checking $ assertFailure "Usage count differs"

-- | Tests that typeclass method usages are extracted.
-- Also includes test for types, as typeclass/instance types are trickier to
-- gather - but this test should be deeper + moved to type tests eventually.
testTypeClass :: AssertionInEnv
testTypeClass = assertXRefsFrom ["basic/TypeClass.hs"] $ do
    -- TODO(robinpalotai): have a separate set of tests where we only
    --   test the reported types of things. For type constructor-like things
    --   maybe we should report their kind instead.
    declAt (5,5) >>= assertAll
        [ singleUsage >=> includesPos (17,5)
          -- Unfortunately class method signatures come from the renamed
          -- (but not typechecked) source, so no fancy forall / contexts.
          -- Could synthetise them if needed, see 'top_matter' in GHC's
          -- HsDecls.
        , userFriendlyTypeIs "a"
        ]
    declAt (9,5) >>= userFriendlyTypeIs "Int"
    declAt (11,1) >>= userFriendlyTypeIs "forall a. Foo a => a -> String"
    -- Test the extracted relations.
    instanceDecl <- declAt (7,10)
    extraAlternateIdSpanContainsPos (7,1) instanceDecl
    declAt (3,7) >>= hasRelation InstantiatesClass instanceDecl
    bind2 (hasRelation ImplementsMethod) (declAt (9,5)) (declAt (5,5))
    -- TODO(robinpalotai): default method support

-- | Tests that calls in instance method bodies are emitted.
-- Also tests that instance methods have an identifier which includes the
-- class + instance name, to make it distinguishable from other instances'
-- method in the caller list.
testTypeClassRef :: AssertionInEnv
testTypeClassRef = assertXRefsFrom ["basic/TypeClassRef.hs"] $ do
    declAt (3,1) >>= singleUsage >>= assertAll
        [ refKindIs Call
        , includesPos (9,13)
        , refContextIs `funk` declAt (9,5)
        ]
    declAt (9,5) >>= extraMethodForInstanceIs "Foo ()"

-- | Tests that C-preprocessed files can be processed, and that declaration
-- locations refer to the original location (not offset by includes etc).
-- This is a smoke test, doesn't check declarations/references originating
-- to/from included files.
testCppInclude :: AssertionInEnv
testCppInclude = assertXRefsFromExtra ["basic/DummyInclude.hs"]
                                      ["basic/CppInclude.hs"] $
    declAt (4,1) >>= singleUsage >>= assertAll
        [ includesPos (8,30)
        , refContextIs `funk` declAt (8,1)
        ]

-- | Smoke test that TemplateHaskell works and references to TH-decls are
-- found. See also b/26456233.
testTemplateHaskellQuotation :: AssertionInEnv
testTemplateHaskellQuotation = assertXRefsFrom
      ["basic/TemplateHaskellQuotation.hs"] $
    -- Location for TH-generated decls is just the whole span of the runQ
    -- block - can live with that.
    declsAt (6,3) >>= \case
        [f, v] -> do
            singleUsage f >>= includesPos (11,11)
            singleUsage v >>= includesPos (11,17)
        _ -> checking $ assertFailure "Expected two decls from TH."

testTemplateHaskellCodeExec :: AssertionInEnv
testTemplateHaskellCodeExec = assertXRefsFrom
      ["basic/TemplateHaskellCodeExec.hs", "basic/UsedByTH.hs"] $
    void $ declAt (8,1)

testTemplateHaskellWerrorOpt :: AssertionInEnv
testTemplateHaskellWerrorOpt = assertXRefsFrom
      ["-O2", "-Werror", "basic/TemplateHaskellQuotation.hs"] $
    return ()

testTemplateHaskellCodeExecFFI :: AssertionInEnv
testTemplateHaskellCodeExecFFI = assertXRefsFrom
      [ "basic/TemplateHaskellCodeExecFFI.hs"
      , "basic/ForeignImport.hs"
      , "basic/ffi.c"
      ] $
    void $ declAt (8,1)

-- TODO(https://github.com/google/haskell-indexer/issues/70): more infra tests.

testForeignImport :: AssertionInEnv
testForeignImport = assertXRefsFrom
      ["basic/ForeignImport.hs", "basic/ffi.c"] $
    -- If we get this far it compiles.
    -- TODO(robinpalotai): assert declaration once supported.
    return ()

testForeignExport :: AssertionInEnv
testForeignExport = assertXRefsFrom ["basic/ForeignExport.hs"] $
    void $ declAt (7,1)

testRtsArgsSkipped :: AssertionInEnv
testRtsArgsSkipped = assertXRefsFrom
      ["+RTS", "-A128M", "-RTS", "basic/ArgRef.hs"] $
    return ()

-- | Contains same name on type and term level, checks if usages go to the
-- correct one.
testDisambiguateTermType :: AssertionInEnv
testDisambiguateTermType = assertXRefsFrom ["basic/TypeVsTerm.hs"] $ do
    declAt (3,10) >>= singleUsage >>= includesPos (6,10)
    declAt (3,6)  >>= singleUsage >>= includesPos (5,11)

-- | Checks that an executable (which usually has package 'main' in GHC) gets a
-- more specific package by including the fallback (set as "dummyPkg" in
-- TestHelper).
testExecutableTickPackage :: AssertionInEnv
testExecutableTickPackage = assertXRefsFrom ["basic/ExecutableMain.hs"] $
    declAt (3,1) >>= declPropEquals
        (getPackage . tickPkgModule . declTick) "dummyPkg_main"

testLocalRef :: AssertionInEnv
testLocalRef = assertXRefsFrom ["basic/LocalRef.hs"] $ do
    declAt (8,5) >>= singleUsage >>= includesPos (6,32)
    declAt (16,5) >>= singleUsage >>= includesPos (14,34)

testRecordRead :: AssertionInEnv
testRecordRead = assertXRefsFrom ["basic/RecordRead.hs"] $ do
    declAt (4,12) >>= usages >>= \case
        -- Don't really caring about this, just a smoke check.
        u1:u2:_ -> do
            includesPos (6,9) u1
            includesPos (10,10) u2
        _ -> checking $ assertFailure "Different use count for ctor read."
    declAt (4,18) >>= usages >>= \case
        [u1, u2, u3, u4, u5, u6, u7, u8] -> do
            -- Simple accessor.
            includesPos (8,12) u1
            -- Unpacked field using record wildcards.
            includesPos (10,14) u2  -- Dotdots.
            includesPos (10,20) u3  -- Unpacked ref.
            -- Unpacked using punning.
            includesPos (12,12) u4  -- The pun.
            includesPos (12,21) u5  -- Unpacked ref.
            -- Unpacked using normal reassignment.
            includesPos (14,16) u6
            -- Wildcard mixed with normal extracted field.
            includesPos (19,20) u7  -- Dotdot.
            includesPos (19,26) u8  -- Unpacked ref.
        other -> checking $ assertFailure (
            "Different use count for field read: " ++ show other)
    -- The reassigned field.
    declAt (14,22) >>= singleUsage >>= includesPos (14, 29)
    -- Complex deconstructed fields should emit a reference from the place
    -- of deconstruction.
    declAt (16,26) >>= singleUsage >>= includesPos (17,24)

testRecordWrite :: AssertionInEnv
testRecordWrite = assertXRefsFrom ["basic/RecordWrite.hs"] $ do
    declAt (4,12) >>= usages >>= \case
        [u1, u2, u3, u4, u5, u6] -> do
            includesPos (9,10) u1
            includesPos (11,26) u2
            includesPos (16,8) u3
            includesPos (21,8) u4
            includesPos (25,14) u5
            includesPos (25,24) u6
        xs -> checking $ assertFailure
            ("Different use count for ctor ref: " ++ show (length xs))
    declAt (5,7) >>= usages >>= \case
        [u1, u2, u3, u4, u5, u6, u7] -> do
            includesPos (11,41) u1  -- Explicit record field assignment.
            --
            includesPos (14,9) u2   -- Wildcard preparation.
            includesPos (16,12) u3  -- Wildcard dots.
            --
            includesPos (19,9) u4   -- Pun preparation.
            includesPos (21,12) u5  -- Pun record write.
            -- Wildcard unpack without use + write with assignment.
            includesPos (25,18) u6  -- Unpack (not really a write).
            includesPos (26,7) u7   -- Assignment.
        xs -> checking $ assertFailure
            ("Different use count for 'foo' write: " ++ show (length xs))
    declAt (6,7) >>= usages >>= \case
        [u1, u2, u3, u4, u5, u6, u7, u8, u9] -> do
            -- Normal field write.
            includesPos (11,32) u1
            -- Wildcard write.
            includesPos (15,9) u2   -- Preparation.
            includesPos (16,12) u3  -- Dots.
            -- Trailing wildcard write.
            includesPos (20,9) u4   -- Preparation.
            includesPos (21,16) u5  -- Trailing wildcard dots.
            -- Record update.
            includesPos (23,16) u6
            -- Unpack from wildcard and write value.
            includesPos (25,18) u7  -- Dots.
            includesPos (27,7) u8   -- Field specifier.
            includesPos (27,13) u9  -- Assigned value.
        xs -> checking $ assertFailure
            ("Different use count for 'bar' write: " ++ show (length xs))
    -- Multi-ctor field update.
    -- TODO(robinpalotai): the non-first ctor's fields should refer that of the
    --   first instead of introducing a decl (as happens in the AST).
    declAt (30,18) >>= singleUsage >>= includesPos (32,23)

-- | Banging fields (and some other things) makes the Wrapper Id to be used
-- instead of the Worker / DataCon Id. If we don't handle this, we wouldn't
-- be able to refer the data constructor.
testDataConWrap :: AssertionInEnv
testDataConWrap = assertXRefsFrom ["basic/DataConWrap.hs"] $
    declAt (3,12) >>= singleUsage >>= includesPos (5,8)

-- | Test that source locations are reported in characters and not bytes. Also
-- verifies that GHC accepts UTF-8 encoding errors as long as those are just in
-- the comments.
--
-- Warning: as the source file contains encoding error, take care if editing it.
-- For example, some text editors refuse to open it at all, and some will
-- replace the bad sequence with something else. Please verify with a hexeditor.
testUtf8 :: AssertionInEnv
testUtf8 = assertXRefsFrom ["basic/Utf8.hs"] $
    declAt (6,1) >>= usages >>= \case
        [u1, u2] -> do
            spanIs (8,7) (8,10) u1
            spanIs (8,14) (8,17) u2
        xs -> checking $ assertFailure
            ("Different use count for unicodey var: " ++ show (length xs))

-- | Test that the module imports are emitted.
testImports :: AssertionInEnv
testImports = assertXRefsFrom ["basic/Imports.hs"] $ do
    importAt (3, 8) "Data.Int"
    importAt (4, 8) "Data.List"

assertRefKind :: ReferenceKind -> TickReference -> ReaderT XRef IO ()
assertRefKind expected tick =
  let actual = refKind tick
   in unless (actual == expected)
        $ checking
        $ assertFailure
        $ show expected ++ " expected; got " ++ show actual

testImportRefs :: AssertionInEnv
testImportRefs = assertXRefsFrom ["basic/ImportDefs.hs", "basic/ImportRefs.hs"]
  $ do
    -- foo
    declAt (9, 1) >>= usages >>= \case
      [u1, u2] -> do
        includesPos (3, 25) u1 -- import statement in ImportRefs.hs
        assertRefKind Import u1
        includesPos (8, 1) u2 -- type signature in ImportDefs.hs
      us -> checking $ assertFailure "Usage count differs for foo"
    -- bar
    declAt (12, 1) >>= usages >>= \case
      [u1, u2] -> do
        includesPos (3, 20) u1 -- import statement in ImportRefs.hs
        assertRefKind Import u1
        includesPos (11, 1) u2 -- type signature in ImportDefs.hs
      us -> checking $ assertFailure "Usage count differs for bar"
    -- FooBar
    declAt (14, 6) >>= usages >>= \case
      [u1, u2, u3] -> do
        includesPos (4, 20) u1
        includesPos (5, 20) u2
        includesPos (6, 20) u3
      us -> checking $ assertFailure "Usage count differs for FooBar"
    -- MkFooBar
    declAt (15, 5) >>= singleUsage >>= includesPos (6, 28)
    -- fbFoo
    declAt (16, 9) >>= singleUsage >>= includesPos (6, 38)
    -- fbBar
    declAt (17, 9) >>= singleUsage >>= includesPos (6, 45)

testImportRefsHiding :: AssertionInEnv
testImportRefsHiding =
  assertXRefsFrom ["basic/ImportDefs.hs", "basic/ImportRefsHiding.hs"]
    $ do
      declAt (12, 1) >>= usages >>= \case
        [u1, u2] -> do
          includesPos (3, 27) u1 -- import statement in ImportRefsHiding.hs
          assertRefKind Ref u1
          includesPos (11, 1) u2 -- type signature in ImportDefs.hs
        us -> checking $ assertFailure "Usage count differs for bar"

-- | Prepares the tests to run with the given test environment.
allTests :: TestEnv -> [Test]
allTests env =
    [ envTestCase "arg-ref" testArgRef
    , envTestCase "recursive-ref" testRecursiveRef
    , envTestCase "data-decl" testDataDecl
    , envTestCase "fun-call" testFunCall
    , envTestCase "typeclass" testTypeClass
    , envTestCase "typeclass-ref" testTypeClassRef
    , envTestCase "cpp-include" testCppInclude
    , envTestCase "th-quotation" testTemplateHaskellQuotation
    , envTestCase "th-code-exec" testTemplateHaskellCodeExec
    , envTestCase "th-code-exec-ffi" testTemplateHaskellCodeExecFFI
    , envTestCase "th-werror-opt" testTemplateHaskellWerrorOpt
    , envTestCase "foreign-import" testForeignImport
    , envTestCase "foreign-export" testForeignExport
    , envTestCase "rts-args-skipped" testRtsArgsSkipped
    , envTestCase "disambiguate-term-type" testDisambiguateTermType
    , envTestCase "executable-package" testExecutableTickPackage
    , envTestCase "local-ref" testLocalRef
    , envTestCase "record-read" testRecordRead
    , envTestCase "record-write" testRecordWrite
    , envTestCase "data-con-wrap" testDataConWrap
    , envTestCase "utf8" testUtf8
    , envTestCase "imports" testImports
    , envTestCase "import-refs" testImportRefs
    , envTestCase "import-refs-hiding" testImportRefsHiding
    ]
  where
    envTestCase name test = testCase name (runReaderT test env)
