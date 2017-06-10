# Kythe verification

The Kythe verifier parses expectation-annotated source code, and correlates it
with the Kythe entries generated from that source.
See [Kythe verifier docs](http://kythe.io/docs/kythe-verifier.html) and
[testing a new indexer](http://kythe.io/docs/schema/writing-an-indexer.html#_testing).

Aim to keep the verifier sources small, as the unification gets slow pretty fast.
Verify only the aspects you care about.

## Running the verifier.

The verifier script expects `ghc_kythe_wrapper` to be on the `PATH`.

For example, if you use stack, do `stack install` in the base directory.

The script also expects the Kythe verifier tool to be in
`/opt/kythe/tools/verifier`. Please look into the script to change, or fix #24.

Change to the `kythe-verification` directory and run `./test.sh`.

## Fixing errors.

In case of error, you'll get something like:

```
Verifying: testdata/basic/TypeDef.hs
Could not verify all goals. The furthest we reached was:
  testdata/basic/TypeDef.hs:4:6-4:7 @
```

This means that the doc assert at line 4, around cols 6-7 was not satisfied.

### Some hints

#### Did you put a span anchor assertion on the right line?

For example,

```
-- - @foo defines/binding Something
-- - @bar ref ARef
foo = bar
```

works, but if you interleave a non-assertion comment, then it already won't
find the anchor:

```
-- - @foo ...
-- - @bar ...
-- I'm a comment.
foo = bar
