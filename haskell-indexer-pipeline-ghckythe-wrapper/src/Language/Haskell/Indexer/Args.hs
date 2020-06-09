
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

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
This module provides the wrapper executable function (wrapperMain) but also
exposes the flag parsing logic so it can be reused by the plugin.
-}
module Language.Haskell.Indexer.Args where

import Data.Bits ((.|.), (.&.), shiftR)
import Data.Bool (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
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
import Language.Haskell.Indexer.Pipeline.GhcKythe (ghcToKythe, pluginContinuation)
import Language.Haskell.Indexer.Util.Path (asTextPath, stripTmpPrefix)
import Language.Haskell.Indexer.Translate

import DynFlags (defaultFatalMessager, defaultFlushOut)
import GHC (defaultErrorHandler)
import GHC.IO.Handle

-- | Command-line flags to control the indexer behavior.
data Flags = Flags
    { flagCorpus :: !Text
    , flagMainPackageRename   :: !(Maybe Text)
    , flagStripPathPrefix     :: !(Maybe Text)
    , flagPrependPathPrefix   :: !(Maybe Text)
    , flagKeepTempPathPrefix  :: !Bool
    , flagOverridePgmP        :: !(Maybe FilePath)
    , flagOverrideLibdir      :: !(Maybe FilePath)
    -- Path to a directory where to place the output files when running
    -- in plugin mode.
    , flagOutput              :: !(Maybe FilePath)
    }

wrapperMain :: IO ()
wrapperMain = do
    (wrapperArgs, rest) <- break (== "--") <$> getArgs
    case rest of
        _:ghcArgs -> withArgs wrapperArgs (execParser opts) >>= index ghcArgs
        _ -> throwIO (ErrorCall "No -- found in args.")
  where
    opts = info (helper <*> flagParser)
        $  header (
               "ghc_wrapper - pretends to be GHC and writes Kythe artifacts. "
               ++ "Options after the first -- are passed on to GHC.")
        <> fullDesc

kythePlugin :: Handle -> (AnalysisOptions -> IO XRef) -> Flags -> IO ()
kythePlugin h f flags =
  indexX (pluginContinuation f h)  [] flags

wrapperParser :: [String] -> IO Flags
wrapperParser opts = withArgs opts (execParser (info flagParser fullDesc))

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
     <*> optional (strOption
             (  long "libdir"
             <> short 'B'
             <> metavar "PATH"
             <> help ("Overrides the GHC libdir.")))
     <*> optional (strOption
            ( long "output"
            <> short 'o'
            <> metavar "INDEX_OUT_DIR"
            <> help ("The directory to write the indices to in plugin mode. "
                     ++ "Normal mode emits entry stream to stdout.")))

index :: [String] -> Flags -> IO ()
index args fs = do
    lock <- newMVar ()
    withErrorHandler $ indexX (ghcToKythe lock) args fs
  where
    withErrorHandler :: IO a -> IO a
    withErrorHandler = defaultErrorHandler defaultFatalMessager defaultFlushOut

indexX
  :: (GhcArgs -> AnalysisOptions -> Raw.VName
        -> (Handle -> [Raw.Entry] -> IO ()) -> IO ())
  -> [String] -> Flags -> IO ()
indexX k args Flags{..} = do
    let ghcArgs = GhcArgs
            { gaArgs = args
            , gaToolOverride = ToolOverride
                { overridePgmP = flagOverridePgmP
                }
            , gaLibdirOverride = OverrideLibdir <$> flagOverrideLibdir
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
    k ghcArgs analysisOptions baseVName collect
  where
    collect handle = mapM_ (\m -> do
        let wire = encodeMessage . Raw.toEntryProto $ m
        B.hPutStr handle . BL.toStrict . Builder.toLazyByteString
                 . varInt . B.length $ wire
        B.hPutStr handle wire)

-- | From proto-lens.
varInt :: Int -> Builder.Builder
varInt n
    | n < 128 = Builder.word8 (fromIntegral n)
    | otherwise = Builder.word8 (fromIntegral $ n .&. 127 .|. 128)
                      <> varInt (n `shiftR` 7)
