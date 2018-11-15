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
      scale is hit (personal idea).
      
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
    
  * Optional: export types to Kythe. This is tricky since Haskell's types are
    complex, and Kythe's schema can't represent them without some loss. Would
    propose to defer this until missing type crosslinking actually hurts
    navigation experience. (https://github.com/google/haskell-indexer/issues/31)
    
  * Any other `haskell-indexer` improvements as well, to increase Haskell feature
    coverage.

* Serving xrefs from local updates:
  * Research/discuss with Kythe team about plans and support, if any.  
  * Prototype. Pretty open-ended at this point, to be updated.
  
## Glossary and links

* Bazel action listener: gets ExtraActionInfo proto for Bazel targets, can identify
  inputs and compiler command based on them.
  https://docs.bazel.build/versions/master/be/extra-actions.html
  
* Bazel aspect: a shadow build graph with custom actions.
  https://docs.bazel.build/versions/master/skylark/aspects.html
  
* Kythe: schema for language agnostic, approximate AST, also tools for serving xref
  quries based on this data.
