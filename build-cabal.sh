#!/usr/bin/env bash

# Script for building Kythe index of Haskell packages using Cabal new-build.
# See usage below.

if (($# < 1)); then
  echo "Usage: $0 package1 package2 ..." >&2
  echo "Env variables with their defaults:" >&2
  echo "  - INDEXER_OUTPUT_DIR=/tmp/indexer-output" >&2
  echo "    Where to put indexing logs and output entries." >&2
  exit 1
fi

# Directory where to build the index. Need to export it, so the GHC wrapper
# executed as subprocess has access to it. Has to be an absolute path,
# since the wrapper will be invoked in various directories.
export INDEXER_OUTPUT_DIR=$(readlink -f "${INDEXER_OUTPUT_DIR:-/tmp/indexer-output}")
[ ! -d "$INDEXER_OUTPUT_DIR" ] && mkdir -p "$INDEXER_OUTPUT_DIR"

# REALGHC is used by the wrapper ghc. Note that it must be set before
# altering the PATH.
export REALGHC=$(which ghc)

project_root=$(cd "$(dirname "$0")"; pwd)

# Build and index the packages
# ============================

# The stack ghc wrapper works well for cabal too.
cabal v2-build "${@:1}" --with-compiler="$project_root/wrappers/stack/ghc"

