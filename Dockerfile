# Use the following command to build image:
#
#   docker build -t hsidx-build .
FROM fpco/stack-build:lts-9.2

# chmod needed if set-user is true in stackage.yaml's docker section.
RUN cd /tmp && \
    wget https://github.com/google/protobuf/releases/download/v3.3.0/protoc-3.3.0-linux-x86_64.zip && \
    unzip protoc-3.3.0-linux-x86_64.zip bin/protoc -d /usr/local && \
    rm protoc-3.3.0-linux-x86_64.zip && \
    chmod 755 /usr/local/bin/protoc
