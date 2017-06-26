#!/bin/bash

# Script for building Kythe index of Haskell packages.
# Usage: bash build-stack.sh /tmp/logs package...

if (($# < 2)); then
  echo "Usage: $0 /tmp/logs package..." >&2
  exit 1
fi

# Directory where to build the index.
export INDEXER_OUTPUT_DIR=$1
mkdir -p "$INDEXER_OUTPUT_DIR"

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
for i in "${@:2}"; do
  stack exec -- ghc-pkg unregister --force "$i" || :
done

# Put stack wrapper ghc script, ghc-pkg (from compiler-bin) and
# ghc_kythe_wrapper (from local-install-root, invoked by wrappers/stack/ghc) on the PATH.
# $(stack path --compiler-bin) is also on the PATH to make --system-ghc pick it instead
# of system ghc (e.g. /usr/bin/ghc).
PATH="$project_root/wrappers/stack:$(stack path --compiler-bin):$PATH:$(stack path --local-install-root)/bin" \
  stack --system-ghc build "${@:2}"
