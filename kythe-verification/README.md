# Kythe verification

The Kythe verifier parses expectation-annotated source code, and correlates it
with the Kythe entries generated from that source.
See [Kythe verifier docs](http://kythe.io/docs/kythe-verifier.html) and
[testing a new indexer](http://kythe.io/docs/schema/writing-an-indexer.html#_testing).

Aim to keep the verifier sources small, as the unification gets slow pretty fast.
Verify only the aspects you care about.
