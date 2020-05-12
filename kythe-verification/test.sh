#!/usr/bin/env bash

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

# TODO(robinp): take these from outer env if present.
GHC_KYTHE="${GHC_KYTHE:-ghc_kythe_wrapper}"
KYTHE_DIR="${KYTHE_DIR:-/opt/kythe}"
VERIFIER="${VERIFIER:-$KYTHE_DIR/tools/verifier}"

BASIC="testdata/basic"

RESULT=0

die() {
  RESULT=-1
  echo "*********************************"
  echo "* THERE WAS AN ERROR, SEE ABOVE *"
  echo "*********************************"
}

if [[ ! -f "$(which $GHC_KYTHE)" ]]; then
    echo "* GHC-Kythe wrapper [$GHC_KYTHE] not found! See Readme."
    exit -1
fi

if [[ ! -f "$VERIFIER" ]]; then
    echo "* Kythe verifier [$VERIFIER] not found! See Readme."
    exit -1
fi

for fut in \
    "$BASIC/Anchors.hs" \
    "$BASIC/CrossRef1.hs $BASIC/CrossRef2.hs" \
    "$BASIC/DataRef.hs" \
    "$BASIC/DocUri.hs" \
    "$BASIC/FunctionArgRef.hs" \
    "$BASIC/ImpExpDefs.hs $BASIC/ImportRefs.hs" \
    "$BASIC/LocalRef.hs" \
    "$BASIC/Module.hs" \
    "$BASIC/ModuleDocUri.hs" \
    "$BASIC/PatSyn.hs" \
    "$BASIC/RecordReadRef.hs" \
    "$BASIC/RecordWriteRef.hs" \
    "$BASIC/RecursiveRef.hs" \
    "$BASIC/TypeclassRef.hs" \
    "$BASIC/TypeDef.hs" \
    "$BASIC/TypeFam.hs" \
    "$BASIC/DataFam.hs" \
    "$BASIC/ClosedFam.hs" \
    "$BASIC/AssocType.hs" \
    "$BASIC/TypeOperators.hs" \
    "$BASIC/TypeVarInSig.hs"
do
  echo "Verifying: $fut"
  $GHC_KYTHE -- $fut 2> /dev/null | $VERIFIER -goal_prefix '-- -' --check_for_singletons=true --ignore_dups $fut \
    || die
done

exit $RESULT
