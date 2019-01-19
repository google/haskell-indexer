# Roadmap

Contains subheaders about larger improvement efforts.

## `rules_haskell` support

Context: [`rules_haskell`](https://github.com/tweag/rules_haskell) supports
building Haskell targets by extending Bazel, openly maintained by Tweag.
It would be useful if `haskell-indexer` supported generating crossreferences for
`rules_haskell` out of the box.

### Goals

Provide crossreference (as Kythe entries) for both `rules_haskell`-managed
and `nixpkgs`-sourced compilations. Latter is used by `rules_haskell` for hackage
imports, instead of hand-rolled source imports.

Crossreference data should be serveable through Kythe's language server, ready to
be consumed by emacs's lsp-mode (with modifications if needed).

Bonus goal: based on local edits, either make partial index updates possible, or
merge local updates with the static backing index at serving time.

Possible future expansions:
  * xref `proto-lens` generated Haskell protobuf code with proto sources.
    (https://github.com/google/haskell-indexer/issues/15)
  * xref Haskell FFI code with C code
    (https://github.com/google/haskell-indexer/issues/18)

#### Rationale

In the non-Bazel world, one might use
[haskell-ide-engine](https://github.com/haskell/haskell-ide-engine) as a
[LSP](https://microsoft.github.io/language-server-protocol/) server over a
stack/cabal workspace, providing editor support (such as type sig requests,
code navigation, completion).

For Bazel/rules_haskell, one can either hand-roll the stack/cabal files in
addition to the BUILD files to run the server. But that is duplicate work, also
there are scaling concerns on large workspaces.

So one way to get LSP support for rules_haskell is to get the Kythe entries
using haskell-indexer, and serve them through [Kythe's language
server](https://github.com/kythe/kythe/tree/master/kythe/go/languageserver).

An alternative would be using GHC's HIE files (expected to land in GHC 8.8) to
write an [LSIF](https://github.com/Microsoft/language-server-protocol/issues/623) index directly, which could be served by `haskell-ide-engine`
without much overhead. This approach is not pursued here, but might be revisited
in the future, especially if LSIF demonstrates a solid handling of cross-project
/ cross-language references (see https://gist.github.com/robinp/76f9d3d91387da5162f773895d4e1d15).

### Tasks

* Decide how compilations will be extracted/analysed.

  * For quick productivity boost, run analysis directly, without extraction.
    * `rules_haskell`: maybe just indicate indexing by passing some global flag
      (if possible with Bazel).  Alternatively use `aspects`.
    * `nixpkgs`: plumb a `should-index` flag to whatever helper is used to define
      a Haskell package. Make `rules_haskell` pass this flag.
    * Kythe entries will be written to a directory defined by the
      `KYTHE_OUTPUT_DIRECTORY` env (convention).

  * Sync with Kythe/Bazel about merits of extraction, vs direct analysis.

    * TLDR extraction helps distributing the analysis workload (analysing machines
      are different than those preparing the build), but is an overhead until that
      scale is hit (personal idea, to be verified).

    * If extraction is needed, decide between `action_listener` vs `aspects`.
      * TLDR: action listeners are older mechanism, for providing info about how
        the compiler was invoked to build a given target. Needs heuristics and/or
        parsing the compiler command-line for non-canonical languages (like
        Haskell). Aspects are newer, more general, and able to retain precise
        info, so less parsing is needed. Should also let accurate access to the
        build chain, leaving less guesswork to the extractor about which inputs
        should be captured.

* Enhancing `haskell-indexer` deficits:

  * Output `MarkedSource` protos, so function signatures can be served by Kythe.
    (https://github.com/google/haskell-indexer/issues/4)

    * Note: would be nice to keep the intermediate Type representation isomorphic
      to HIE's HieType, so switching haskell-indexer to use HIE as source later
      is smooth (also to avoid duplicate work).

  * Optional: export types to Kythe. This is tricky since Haskell's types are
    complex, and Kythe's schema can't represent them without some loss. Would
    propose to defer this until missing type crosslinking actually hurts
    navigation experience. (https://github.com/google/haskell-indexer/issues/31)

    * Note: Kythe team is open to schema suggestions, but we need a good usecase
      first before engineering a lot (is there any static analysis that would
      benefit from fine-grained Kythe-level types?)

  * Any other `haskell-indexer` improvements as well, to increase Haskell feature
    coverage.

    * Note: in the advent of [HIE](https://ghc.haskell.org/trac/ghc/wiki/HIEFiles),
      we shouldn't enhance Haskell source features, but rather wait to switch to
      HIE, so we can get rid of our custom GHC API-based mining. What we should
      concentrate on are rather cross-language features (that need effort regardless
      of HIE).

* Serving xrefs from local updates:
  * Research/discuss with Kythe team about plans and support, if any.
  * Prototype. Pretty open-ended at this point, to be updated.

## Glossary and links

* Bazel action listener: gets ExtraActionInfo proto for Bazel targets, can
  identify inputs and compiler command based on them.
  https://docs.bazel.build/versions/master/be/extra-actions.html

* Bazel aspect: a shadow build graph with custom actions.
  https://docs.bazel.build/versions/master/skylark/aspects.html

* Kythe: schema for language agnostic, approximate AST, also tools for serving
  xref quries based on this data.

* HIE: Haskell Interface Extended files, containing a simplified AST, with
  scopes and (constrained) types already resolved. Likely suitable to use as a
  haskell-indexer backend.  See https://ghc.haskell.org/trac/ghc/wiki/HIEFiles
  and https://github.com/ghc/ghc/tree/master/compiler/hieFile.

