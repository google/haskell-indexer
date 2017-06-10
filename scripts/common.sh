#!/bin/bash

# Library to include in all wrapper runners.

fail() {
    (>&2 echo "$1")
    exit 1
}

# Serve the index
# ===============
# It's probably more efficient to cat them together, but this way we see
# if a given one is corrupted for any reason.

serve_index() {
    set -u
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
}
