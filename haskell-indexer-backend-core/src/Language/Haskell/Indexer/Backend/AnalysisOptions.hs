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

{-# LANGUAGE OverloadedStrings #-}

{- | Mostly compiler-agnostic options that can affect the analysis. -}
module Language.Haskell.Indexer.Backend.AnalysisOptions
    ( AnalysisOptions(..)
    , defaultAnalysisOptions
    ) where

import Data.Text (Text)

-- | External factors affecting the analysis.
data AnalysisOptions = AnalysisOptions
    { aoMainPkgFallback     :: !Text
      -- ^ GHC needs package = 'main' internally when compiling binaries with a
      -- Main module, but we would like to report distinct packages to the
      -- frontend. So if the package reported by GHC is 'main', this gets
      -- substituted.
      -- Note: this is GHC specific, but is not strictly needed by GHC, rather
      -- by sane output. Thus here.
      -- TODO(robinpalotai): What happens with multiple executables per package?
      --                     Should we invent a virtual module name based on
      --                     path for example?
    , aoDemanglePackageName :: !(Text -> Text)
      -- ^ If the packages name extracted from the build system are mangled for
      -- some reason, this function can be used to demangle them.
    , aoFilePathTransform   :: !(Text -> Text)
      -- ^ Can be used to transform filepaths if needed. For example, can be
      -- used to drop prefixes for temporary workdirs, or corpus-wide static
      -- prefixes. The result should be closely aligned with what programmers
      -- recognize as the ideal path for any given file. The transformed path
      -- will be the one referenced by frontends when emitting data.
    }

defaultAnalysisOptions :: AnalysisOptions
defaultAnalysisOptions = AnalysisOptions "main" id id
