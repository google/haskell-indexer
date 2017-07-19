#!/bin/bash
if (($# != 2)); then
  echo "Usage: $0 /tmp/logs localhost:8080" >&2
  exit 1
fi

INDEXER_OUTPUT_DIR=$1
KYTHE_DIR="${KYTHE_DIR:-/opt/kythe}"

# Serve the index
# ===============
# Delete old Kythe GraphStore and Kythe serving tables.
rm -fr "$INDEXER_OUTPUT_DIR"/{gs,tbl}


# It's probably more efficient to cat them together, but this way we see
# if a given one is corrupted for any reason.
for e in "$INDEXER_OUTPUT_DIR"/*.entries
do
  echo " * ${e}"
  $KYTHE_DIR/tools/write_entries --graphstore "$INDEXER_OUTPUT_DIR/gs" < "$e"
done
echo "== Converting to serving tables."
$KYTHE_DIR/tools/write_tables \
    --graphstore "$INDEXER_OUTPUT_DIR/gs" \
    --out "$INDEXER_OUTPUT_DIR/tbl" \
    --compress_shards
echo "== Starting HTTP server."
echo " * Click the ::/ in the top-left!"
$KYTHE_DIR/tools/http_server \
    --serving_table "$INDEXER_OUTPUT_DIR/tbl" \
    --listen "$2" \
    --public_resources $KYTHE_DIR/web/ui
