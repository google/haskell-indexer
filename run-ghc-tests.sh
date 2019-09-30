#!/usr/bin/env bash
set -euxo pipefail

for i in stack-ghc865.yaml
do
  export STACK_YAML=$i
  stack test haskell-indexer-backend-ghc
  stack install
  pushd kythe-verification
  ./test.sh
  popd
done
banner Success!
