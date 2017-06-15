The haskell-indexer package provides libs for preprocessing Haskell source code
into a representation for easy entity cross-referencing, as well as a frontend
for emitting entities for the [Kythe](https://kythe.io) indexing schema.

This is not an official Google product.

# Installation (Linux)

## Stack

Download Stack from http://docs.haskellstack.org

## Kythe

If you want to use the Kythe frontend, download a [Kythe
release](https://github.com/google/kythe/releases) and unpack it.

```
tar xzf kythe-v0.0.26.tar.gz -C /opt/
rm -r /opt/kythe
ln -s /opt/kythe-v0.0.26 /opt/kythe
chmod -R 755 /opt/kythe/web/ui  # It misses permission by default.
```

Version `v0.0.26` is verified to work with Haskell indexer.

## Protoc 3

Download the latest [Proto compiler 3
release](https://github.com/google/protobuf/releases), unpack it and place the
binary in the PATH.

```
unzip -j protoc-*-linux-x86_64.zip bin/protoc -d /usr/local/bin/
```

> If you use have Nix installed and you use `stack --nix`, you do not need to do
> this.

# Build the project

Use the following to build and run tests:

```
git clone --recursive https://github.com/google/haskell-indexer.git
cd haskell-indexer
# 8.17 -> GHC 8.0.2
# 6.30 -> GHC 7.10.3
export STACK_YAML=$(readlink -f stack-8.17.yaml)
stack build && stack test
# To test Kythe frontend:
pushd kythe-verification; stack install && ./test.sh; popd
```

# Demo

To index a few packages and serve the index, run:

```
./build-stack.sh /tmp/logs mtlparse cpu
```

The script temporarily replaces the system GHC with
`wrappers/stack-docker/fake-stack/ghc` script, does the indexing and serves the
built index at `localhost:8080`.

If you get empty index, look at `/tmp/logs/*.stderr` files about possible
indexing errors. Also make sure that you `/tmp/logs/*.entries` files are not
empty. If they are, there was some trouble with indexing.

## Indexing using Docker

If you plan to use the Dockerized build feature of stack, please install
Docker. It is also advised to set up a docker wrapper script by following the
instructions at the [stack Docker
security](https://docs.haskellstack.org/en/stable/docker_integration/#security)
section.

The docker image has all C library dependencies so it's possible to use it to
index the whole Stackage snapshot. See `stack-build-docker.sh` for a
comprehensive example of indexing a stackage snapshot, and serving a Kythe
index.
