The haskell-indexer package provides libs for preprocessing Haskell source code
into a representation for easy entity cross-referencing, as well as a frontend
for emitting entities for the [Kythe](https://kythe.io) indexing schema.

This is not an official Google product.

# Instructions

## Prerequisites

### Protoc 3

First, install the `protoc` binary somewhere in your PATH. You can get it by
downloading the corresponding file for your system from
https://github.com/google/protobuf/releases. The corresponding file will be
named something like `protoc-*-.zip`.

Note: you'll need at least version 3 of the protobuf compiler. Check the version
if you intend to install the one provided by your package manager.

### Kythe

If you want to use the Kythe frontend, it is advised to have the Kythe tools
installed.

Please install a [Kythe release](https://github.com/google/kythe/releases) to
`/opt/kythe` for this. Version `v0.0.26` was tested.

### Docker

If you plan to use the Dockerized build feature of stack, please install
Docker. It is also advised to set up a docker wrapper script by following the
instructions at the [stack Docker
security](https://docs.haskellstack.org/en/stable/docker_integration/#security)
section.

## Build

Use the following to build and run tests:

    git clone --recursive https://github.com/google/haskell-indexer.git
    cd haskell-indexer
    stack build

## Demo

See `stack-build-docker.sh` for a comprehensive example of indexing a stackage
snapshot, then generating and serving a Kythe index.
