#!/bin/bash

# Copyright 2017 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

OUT=/tmp/kythe-entries

build_image() {
  echo "== Building build image with protoc."
  #docker build -t hsidx-build .
  echo "== Building ghc kythe wrapper."
  #stack --docker build
  WRAPPER=$(stack --docker path | grep local-install-root | cut -d: -f2 | sed 's/^\s*//')/bin/ghc_kythe_wrapper
  echo "== Wrapper built at $WRAPPER."
  pushd wrappers/stack-docker
  echo "== Building build image with fake ghc included."
  # Need to copy since docker build is relative and can't go outside.
  cp $WRAPPER ./wrapper
  docker build -t fake-build --build-arg wrapper_path=wrapper .
  rm wrapper
  echo "== Done, happy indexing!"
  popd
}

do_index() {
  echo "== Going to index a sample toy package to see if it works."
  echo "== Note! if you want to index 'everything', best is to remove "
  echo "==       ~/.stack otherwise cached deps won't be reprocessed."
  echo "==       (--force-dirty only affects local packages)."
  echo "==       Also change the build target to 'everything'."
  pushd wrappers/stack-docker
  [ ! -e $OUT ] && mkdir $OUT
  # change 'test-package' to 'everything' for more fun.
  stack --docker \
      --docker-image fake-build \
      --docker-mount "$OUT:/logs" \
      --stack-yaml stack-lts-6.30.yaml \
      build \
      --force-dirty \
      test-package
  echo "== Done, entries and logs in $OUT."
  popd
}

serve() {
  echo "== Writing entries to Kythe graphstore."
  pushd wrappers/stack-docker
  # It's probably more efficient to cat them together, but this way we see
  # if a given one is corrupted for any reason.
  for e in $(ls $OUT/*entries)
  do
    echo " * $e"
    cat $e | /opt/kythe/tools/write_entries --graphstore $OUT/gs
  done
  echo "== Converting to serving tables."
  /opt/kythe/tools/write_tables \
      --graphstore $OUT/gs \
      --out $OUT/tbl \
      --compress_shards
  echo "== Starting HTTP server."
  echo " * Click the ::/ in it's top-left!"
  /opt/kythe/tools/http_server \
      --serving_table $OUT/tbl \
      --listen 0.0.0.0:8080 \
      --public_resources /opt/kythe/web/ui
  popd
}

build_image
do_index
serve
