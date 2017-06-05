The haskell-indexer package provides libs for preprocessing Haskell source code
into a representation for easy entity cross-referencing, as well as a frontend
for emitting entities for the [Kythe](https://kythe.io) indexing schema.

This is not an official Google product.

# Installation (Linux)

## Install Haskell compiler & tools (GHC & Stack).

See: https://www.haskell.org/downloads#platform

If you only miss Stack, get it from here: http://docs.haskellstack.org/en/stable/README/

## Kythe

If you want to use the Kythe frontend, download a [Kythe
release](https://github.com/google/kythe/releases) and unpack it.

```
tar xzf kythe-v*.tar.gz
rm -rf /opt/kythe
mv kythe-v*/ /opt/kythe
```

Version `v0.0.26` is verified to work with Haskell indexer.

## Protoc 3

Download the latest [Proto compiler 3
release](https://github.com/google/protobuf/releases), unpack it and place the
binary in the PATH.

```
unzip -j protoc-*-linux-x86_64.zip bin/protoc -d /usr/local/bin/
```

# Build the project

Use the following to build and run tests:

```
git clone --recursive https://github.com/google/haskell-indexer.git
cd haskell-indexer
stack build
```

# Demo

To index a few packages and serve the index, run:

```
./build-stack.sh mtlparse cpu
```

The script temporarily replaces the system GHC with
`wrappers/stack-docker/fake-stack/ghc` script, does the indexing and serves the
built index at `localhost:8080`.

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