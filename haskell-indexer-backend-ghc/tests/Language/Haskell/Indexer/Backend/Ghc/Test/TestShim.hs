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

-- | Shim to easily run Translate layer tests using standard GHC environment.
module Language.Haskell.Indexer.Backend.Ghc.Test.TestShim (runTests) where

import Test.Framework (defaultMain, Test)

import Language.Haskell.Indexer.Backend.GhcArgs (defaultGhcArgs)
import Language.Haskell.Indexer.Backend.Ghc.Test.TestHelper (TestEnv(..))

runTests :: (TestEnv -> [Test]) -> IO ()
runTests allTests = do
    let env = TestEnv "testdata" defaultGhcArgs
    defaultMain (allTests env)
