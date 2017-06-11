#!/bin/bash

# Script for building Kythe index of Haskell packages.
# Usage: bash build-stack.sh /tmp/logs package...

fail() {
  (>&2 echo "$1")
  exit 1
}

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
# ===============
# It's probably more efficient to cat them together, but this way we see
# if a given one is corrupted for any reason.
for e in $(ls ${INDEXER_OUTPUT_DIR}/*entries)
do
  echo " * ${e}"
  cat ${e} | /opt/kythe/tools/write_entries --graphstore ${INDEXER_OUTPUT_DIR}/gs
done
echo "== Converting to serving tables."
/opt/kythe/tools/write_tables \
    --graphstore ${INDEXER_OUTPUT_DIR}/gs \
    --out ${INDEXER_OUTPUT_DIR}/tbl \
    --compress_shards
echo "== Starting HTTP server."
echo " * Click the ::/ in it's top-left!"
/opt/kythe/tools/http_server \
    --serving_table ${INDEXER_OUTPUT_DIR}/tbl \
    --listen 0.0.0.0:8080 \
    --public_resources /opt/kythe/web/ui
# The index is served at localhost:8080
