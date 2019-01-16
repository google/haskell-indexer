#!/bin/env bash
set -eu

if [ $# -lt 3 ]
then
	echo
	echo USAGE
	echo
	echo "    $0 OUTDIR GHC_PATH PLUGIN_DB"
    echo
	echo "   " Generates a wrapper for a ghc compiler in OUTDIR that indexes 
    echo "   " using the given compiler.
	echo
	echo EXAMPLE
	echo
	echo "    $0 ghc_wrapper" '"$(stack path --compiler-exe)" \'
	echo '    "$(stack path --snapshot-pkg-db)"'
	echo
	exit 1
fi

RELATIVE_OUTDIR="$1"
mkdir -p "$RELATIVE_OUTDIR"
OUTDIR="$(cd "$RELATIVE_OUTDIR"; pwd)"
GHC="$2"
GHC_DIR="$(dirname "$2")"
PLUGIN_DB="$3"
GHC_VERSION=$("$GHC" --numeric-version)
GHC_WRAPPER="$OUTDIR/ghc-$GHC_VERSION"

# symlink all executables in the bin folder
for f in $(ls "$GHC_DIR")
do
	ln -sf "$GHC_DIR/$f" "$OUTDIR/$f"
done
# Remove the symlink to ghc to avoid modifying the target
rm -f "$GHC_WRAPPER"
# Write the ghc wrapper
cat > "$GHC_WRAPPER" <<EOF
#!/bin/env bash
if [ "\${INDEXER_PLUGIN_ENABLE:-}" != 0 ]
then
    PLUGIN="\
-plugin-package haskell-indexer-plugin \
-fplugin Haskell.Indexer.Plugin \
-fplugin-opt Haskell.Indexer.Plugin:-o \
-fplugin-opt Haskell.Indexer.Plugin:\${INDEXER_OUTPUT_DIR:-/tmp/indexer-output}"
else
    PLUGIN=
fi
"$GHC_DIR/ghc" \$@ -package-db "$PLUGIN_DB" \$PLUGIN
EOF
chmod +x "$GHC_WRAPPER"
ln -sf "$GHC_WRAPPER" "$OUTDIR/ghc"
