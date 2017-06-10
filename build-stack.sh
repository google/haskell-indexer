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
export REALGHC=$(stack path --compiler-exe)

# Build and index the packages
# ============================
# Put stack wrapper ghc script, ghc-pkg (from compiler-bin) and
# ghc_kythe_wrapper (from local-install-root) on the PATH. Note that stack
# wrapper ghc script replaces the system ghc.
PATH=$PWD/wrappers/stack:$(stack path --compiler-bin):$PATH:$(stack path --local-install-root)/bin \
  stack --system-ghc build --no-nix --force-dirty ${@:2}
[[ $? != 0 ]] && fail "Indexing failed!"

# Serve the index
serve_index
