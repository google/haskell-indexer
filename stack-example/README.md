This setup shows how to index haskell packages using `stack`
and the [haskell-indexer-plugin][haskell-indexer-plugin].

[haskell-indexer-plugin]: https://github.com/google/haskell-indexer

First build the plugin.
```bash
$ stack build haskell-indexer-plugin
```

Then generate a ghc wrapper script. This script will pass
to GHC all the flags needed to enable the indexer plugin.
```bash
$ ./gen-ghc-wrapper.sh

USAGE

    ./gen-ghc-wrapper.sh OUTDIR GHC_PATH PLUGIN_DB

Generates a wrapper for a ghc compiler in OUTDIR that indexes
using the given compiler.

EXAMPLE

    ./gen-ghc-wrapper.sh ghc_wrapper "$(stack path --compiler-exe)" \
    "$(stack path --snapshot-pkg-db)"

$ ./gen-ghc-wrapper.sh ghc_wrapper "$(stack path --compiler-exe)" \
  "$(stack path --snapshot-pkg-db)"
```

Now we can index a package and all of its dependencies with
```bash
$ export INDEXER_OUTPUT_DIR=/tmp/indexer-output
$ PATH=$(pwd)/ghc_wrapper:$PATH \
  STACK_ROOT=$HOME/.stack-indexer stack --system-ghc build <pkg>
```

Packages which use Safe Haskell [can't be indexed currently][sh-issue].
They can be skipped with
```bash
$ INDEXER_PLUGIN_ENABLE=0 PATH=$(pwd)/ghc_wrapper:$PATH \
  STACK_ROOT=$HOME/.stack-indexer stack --system-ghc build <pkg>
```

To see the result of indexing all produced files need to be collected
into a serving table. This requires installing [kythe][kythe-install]
in advance.

Then you can use [this script][server.sh] to start the web ui.
It picks the indexer output from the environment variable
`INDEXER_OUTPUT_DIR`.
```bash
$ serve.sh localhost:8080
```

[server.sh]: https://github.com/google/haskell-indexer/blob/master/serve.sh
[kythe-install]: https://github.com/google/haskell-indexer#kythe


### Limitations

* You shall not index the plugin itself `haskell-indexer-plugin`.
* You shall not index boot libraries. But possibly, they can be
  indexed if they are added as packages to the `stack.yaml` file.
* [You shall not index packages which use Safe Haskell][sh-issue].

[sh-issue]: https://ghc.haskell.org/trac/ghc/ticket/15920
