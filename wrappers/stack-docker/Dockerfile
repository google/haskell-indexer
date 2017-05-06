# Build with
#   docker build -t fake-build \
#       --build-arg wrapper_path=path/to/bin/ghc_kythe_wrapper .
FROM fpco/stack-build:lts-6.30

ARG wrapper_path
COPY $wrapper_path /usr/local/bin/ghc_kythe_wrapper

# The original is just a symlink pointing to ghc-7.10.3.
# The fake ghc will call the latter explicitly.
COPY fake-docker/ghc /usr/local/bin/fhc

# chmod needed if set-user is true in stackage.yaml's docker section.
RUN mv /opt/stackage/lts-6/ghc/lib/ghc-7.10.3/bin/ghc /opt/stackage/lts-6/ghc/lib/ghc-7.10.3/bin/ohc && \
    ln -s /usr/local/bin/fhc /opt/stackage/lts-6/ghc/lib/ghc-7.10.3/bin/ghc && \
    chmod 755 /usr/local/bin/fhc /usr/local/bin/ghc_kythe_wrapper
