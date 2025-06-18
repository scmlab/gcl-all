# Stage 1: Builder stage
FROM ghcr.io/lcamel/gcl-language-server-devcontainer:latest AS builder

USER vscode

# this layer is for caching build dependencies
WORKDIR /tmp/cache-build-deps
COPY gcl/stack.yaml gcl/stack.yaml.lock gcl/package.yaml ./
RUN /home/vscode/.ghcup/bin/stack build --only-dependencies && rm -rf /tmp/cache-build-deps
# now we have ghc and compiled dependencies cached in ~/.stack

COPY --chown=vscode:vscode . /workspaces/gcl-all
WORKDIR                      /workspaces/gcl-all
RUN bash -x build.sh


# Stage 2: Runtime stage
FROM mcr.microsoft.com/devcontainers/base:ubuntu-22.04 AS gcl
# Copy built artifacts from the builder stage
COPY --from=builder --chown=vscode:vscode /home/vscode/.local/bin/gcl /home/vscode/.local/bin/
COPY --from=builder --chown=vscode:vscode /home/vscode/*.vsix         /home/vscode/
