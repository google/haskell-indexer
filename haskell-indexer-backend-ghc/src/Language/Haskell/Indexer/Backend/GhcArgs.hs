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

module Language.Haskell.Indexer.Backend.GhcArgs
    ( GhcArgs(..)
    , ToolOverride(..)
    , defaultGhcArgs
    ) where

-- | Args to call GHC with, to perform the compilation.
data GhcArgs = GhcArgs
    { gaToolOverride :: !ToolOverride
    , gaArgs         :: [String]
    , gaLibdirPrefix :: !FilePath
      -- ^ Gets prepended to libdir path reported by ghc-paths. Needed if
      -- runtime location of libdir differs from the reported one.
    }

-- | Lets selective override of various tools used during compilation.
data ToolOverride = ToolOverride
    { overridePgmP :: !(Maybe FilePath)
      -- ^ Override the preprocessor if needed. Only overrides the binary, but
      -- keeps whatever flags were set on it originally (from the GHC command
      -- line).
    }

defaultGhcArgs :: GhcArgs
defaultGhcArgs = GhcArgs (ToolOverride Nothing) [] ""
