#
# Install ghc and compile dependencies in ~/.stack
# This stage seldom changes and is also used for the dev container
#
FROM ghcr.io/lcamel/gcl-all-builder-base:latest AS builder-deps
USER vscode
WORKDIR /tmp/cache-build-deps
COPY gcl/stack.yaml gcl/stack.yaml.lock gcl/package.yaml ./
RUN source $HOME/.ghcup/env && stack build --only-dependencies && rm -rf /tmp/cache-build-deps


#
# Build the language server and the extension
#
FROM builder-deps AS builder
USER vscode
COPY --chown=vscode:vscode . /workspaces/gcl-all
WORKDIR                      /workspaces/gcl-all
RUN bash -x build.sh


#
# Runtime stage
#
FROM mcr.microsoft.com/devcontainers/base:ubuntu-22.04 AS gcl
# Copy built artifacts from the builder stage
COPY --from=builder --chown=vscode:vscode /home/vscode/.local/bin/gcl /home/vscode/.local/bin/
COPY --from=builder --chown=vscode:vscode /home/vscode/*.vsix         /home/vscode/
