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
{-# LANGUAGE RecordWildCards #-}
{- | Runs the indexer and emits serialized Kythe storage protos to stdout.
The individual protos are prefixed by a varint-encoded binary length.

Example: Indexing a module.

    $ ghc_kythe_wrapper -c mycorpus -P gcc -- A.hs \
          | /opt/kythe/tools/write_entries -graphstore /tmp/gs
    $ /opt/kythe/tools/http_server \
          -graphstore /tmp/gs \
          -listen 0.0.0.0:8000 \
          -public_resources /opt/kythe/web/ui

Note: the arguments after the '--' are standard GHC arguments.
-}
module Main where

import Data.Bits ((.|.), (.&.), shiftR)
import Data.Bool (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.ProtoLens (encodeMessage)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Concurrent.MVar (newMVar)
import Control.Exception (throwIO, ErrorCall(..))
import Options.Applicative
import System.Environment (getArgs, withArgs)

import Language.Haskell.Indexer.Backend.AnalysisOptions (AnalysisOptions(..))
import Language.Haskell.Indexer.Backend.GhcArgs
import qualified Language.Kythe.Schema.Raw as Raw
import qualified Language.Kythe.Schema.Raw.Proto as Raw
import Language.Haskell.Indexer.Pipeline.GhcKythe (ghcToKythe)
import Language.Haskell.Indexer.Util.Path (asTextPath, stripTmpPrefix)

-- | Command-line flags to control the indexer behavior.
data Flags = Flags
    { flagCorpus :: !Text
    , flagMainPackageRename   :: !(Maybe Text)
    , flagStripPathPrefix     :: !(Maybe Text)
    , flagPrependPathPrefix   :: !(Maybe Text)
    , flagKeepTempPathPrefix  :: !Bool
    , flagOverridePgmP        :: !(Maybe FilePath)
    }

main :: IO ()
main = do
    (wrapperArgs, rest) <- break (== "--") <$> getArgs
    case rest of
        _:ghcArgs -> withArgs wrapperArgs (execParser opts) >>= index ghcArgs
        _ -> throwIO (ErrorCall "No -- found in args.")
  where
    opts = info (helper <*> flagParser)
        $  header (
               "ghc_kythe_wrapper - pretends to be GHC and writes Kythe "
               ++ "artifacts. Options after the first -- are passed on to GHC.")
        <> fullDesc

flagParser :: Parser Flags
flagParser = Flags
     <$> (T.pack <$> strOption
             (  long "corpus"
             <> short 'c'
             <> help "The name of the Kythe corpus being indexed."
             <> value ""
             <> showDefault))
     <*> optional (T.pack <$> strOption
             (  long "rename_main"
             <> short 'm'
             <> metavar "PACKAGE"
             <> help ("Changes the 'main' package name when emitting entries. "
                      ++ "Useful when indexing multiple binaries.")))
     <*> optional (T.pack <$> strOption
             (  long "drop_path_prefix"
             <> metavar "PREFIX"
             <> help "Strip the given prefix from emitted filepaths."))
     <*> optional (T.pack <$> strOption
             (  long "prepend_path_prefix"
             <> metavar "PREFIX"
             <> help "Prepends prefix to emitted filepaths (after stripping, if any)."))
     <*> switch
             (  long "keep_path_tmp_prefix"
             <> help ("If set, won't apply the default removal of temporary "
                      ++ "dirs from emitted path prefixes."))
     <*> optional (strOption
             (  long "pgmP_binary"
             <> short 'P'
             <> metavar "PATH"
             <> help ("Overrides the preprocessor binary, but keeps it's "
                      ++ "options. Note: other tools can still be overriden "
                      ++ "by passing the regular -pgmX GHC options.")))

index :: [String] -> Flags -> IO ()
index args Flags{..} = do
    lock <- newMVar ()
    let ghcArgs = defaultGhcArgs
            { gaArgs = args
            , gaToolOverride = ToolOverride
                { overridePgmP = flagOverridePgmP
                }
            }
        analysisOptions = AnalysisOptions
            { aoMainPkgFallback = fromMaybe "main" flagMainPackageRename
            , aoDemanglePackageName = id
            , aoFilePathTransform
                  = customPrepend
                  . customStrip
                  . bool (asTextPath stripTmpPrefix) id flagKeepTempPathPrefix
            }
          where customStrip p = fromMaybe p $ do
                  prefix <- flagStripPathPrefix
                  T.stripPrefix prefix p
                customPrepend p = fromMaybe p . fmap (<> p)
                                $ flagPrependPathPrefix
        baseVName = Raw.VName "" flagCorpus "" "" "haskell"
    ghcToKythe lock ghcArgs analysisOptions baseVName collect
  where
    collect = mapM_ (\m -> do
        let wire = encodeMessage . Raw.toEntryProto $ m
        B.putStr . BL.toStrict . Builder.toLazyByteString
                 . varInt . B.length $ wire
        B.putStr wire)

-- | From proto-lens.
varInt :: Int -> Builder.Builder
varInt n
    | n < 128 = Builder.word8 (fromIntegral n)
    | otherwise = Builder.word8 (fromIntegral $ n .&. 127 .|. 128)
                      <> varInt (n `shiftR` 7)
