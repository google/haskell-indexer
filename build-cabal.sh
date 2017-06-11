#!/bin/bash

# Script for building Kythe index of Haskell packages.
# Usage: bash build-stack.sh /tmp/logs package...

# Figure out a path to the directory where this file resides.
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

. $DIR/scripts/common.sh

# Directory where to build the index.
export INDEXER_OUTPUT_DIR=$1

# REALGHC is used by stack wrapper ghc. Note that it must be set before
# altering the PATH.
export REALGHC=$(readlink -m $(which ghc))

INDEXER_BIN=$(
    cd $DIR
    echo $(stack path --local-install-root)/bin
)

INDEXER_GHC=$(
    cd $DIR
    readlink -m $(stack path --compiler-exe)
)

if [[ "$REALGHC" != "$INDEXER_GHC" ]]; then
    fail "GHC versions used to build the indexer and to be used by cabal should match: $INDEXER_GHC vs $REALGHC"
fi

# Build and index the packages
# ============================
# Put stack wrapper ghc script, ghc-pkg (from compiler-bin) and
# ghc_kythe_wrapper (from local-install-root) on the PATH. Note that stack
# wrapper ghc script replaces the system ghc.
PATH=$PATH:$INDEXER_BIN cabal new-build --with-compiler=$DIR/wrappers/cabal/ghc
[[ $? != 0 ]] && fail "Indexing failed!"

# Serve the index
serve_index
