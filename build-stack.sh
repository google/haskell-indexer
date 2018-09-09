#!/bin/bash

# Script for building Kythe index of Haskell packages.
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

# REALGHC is used by stack wrapper ghc. Note that it must be set before
# altering the PATH.
export REALGHC=$(stack path --compiler-exe)

project_root=$(cd "$(dirname "$0")"; pwd)

# Build and index the packages
# ============================
# `stack build` does not rebuild packages if they have been registered in the
# snapshot database (something like ~/.stack/snapshots/x86_64-linux/lts-8.17/8.0.2/pkgdb),
# thus we unregister the packages first to force rebuilding.
# Note: `ghc-pkg unregister` does not unregister dependencies, so dependencies
# won't be reindexed unless explicitly specified in the command line.
#
# Note: disabling for now, as it was not a reliable way to force rebuild, also can break
# the package database.
#
# for i in "${@:1}"; do
#   stack exec -- ghc-pkg latest "$i" &> /dev/null && stack exec \
#     -- ghc-pkg unregister --force "$i" || :
# done

# Put stack wrapper ghc script, ghc-pkg (from compiler-bin) and
# ghc_kythe_wrapper (from local-install-root, invoked by wrappers/stack/ghc) on the PATH.
# $(stack path --compiler-bin) is also on the PATH to make --system-ghc pick it instead
# of system ghc (e.g. /usr/bin/ghc).
PATH="$project_root/wrappers/stack:$(stack path --compiler-bin):$PATH:$(stack path --local-install-root)/bin" \
  stack --system-ghc --force-dirty build "${@:1}"
