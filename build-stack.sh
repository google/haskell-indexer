#!/bin/bash

# Script for building Kythe index of Haskell packages.
# Usage: bash build-stack.sh package...

fail() {
  (>&2 echo "$1")
  exit 1
}

# Directory where to build the index.
export OUT=/tmp/logs

# REALGHC is used by fake-stack ghc. Note that it must be set before altering
# the PATH.
export REALGHC=$(stack path --compiler-exe)
# Put ghc_kythe_wrapper, ghc-pkg and fake-stack ghc script on the PATH. Note
# that fake-stack ghc script replaces the system ghc.
PATH=$PWD/wrappers/stack-docker/fake-stack:$(stack path --compiler-bin):$PATH:$(stack path --local-install-root)/bin

# Build and index the packages
# ============================
stack --system-ghc build --force-dirty $@
[[ $? != 0 ]] && fail "Indexing failed!"

# Serve the index
# ===============
# It's probably more efficient to cat them together, but this way we see
# if a given one is corrupted for any reason.
for e in $(ls ${OUT}/*entries)
do
  echo " * ${e}"
  cat ${e} | /opt/kythe/tools/write_entries --graphstore ${OUT}/gs
done
echo "== Converting to serving tables."
/opt/kythe/tools/write_tables \
    --graphstore ${OUT}/gs \
    --out ${OUT}/tbl \
    --compress_shards
echo "== Starting HTTP server."
echo " * Click the ::/ in it's top-left!"
/opt/kythe/tools/http_server \
    --serving_table ${OUT}/tbl \
    --listen 0.0.0.0:8080 \
    --public_resources /opt/kythe/web/ui
# The index is served at localhost:8080
