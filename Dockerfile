#
# Installs NVM and Node.js on top of the base Haskell image.
#
FROM ghcr.io/lcamel/haskell-devcontainer:stackage-lts-24.11 AS haskell-and-nodejs
USER vscode
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash && \
    export NVM_DIR="$HOME/.nvm" && \
    . "$NVM_DIR/nvm.sh" && \
    echo "nvm version: $(nvm --version)" && \
    nvm install 24 && \
    nvm use 24 && \
    echo '. $NVM_DIR/nvm.sh' >> $HOME/.bashrc


#
# Precompiles project dependencies to ~/.stack for caching.
# This stage rarely changes.
#
FROM haskell-and-nodejs AS prebuilt-haskell-dependencies
USER vscode
WORKDIR /tmp/cache-build-deps
COPY gcl/stack.yaml gcl/stack.yaml.lock gcl/package.yaml ./
RUN . $HOME/.ghcup/env && stack build --only-dependencies && rm -rf /tmp/cache-build-deps


#
# Builds the gcl binary and the VSCode extension .vsix.
# This is an intermediate stage - only artifacts are extracted, the stage itself is discarded.
#
FROM prebuilt-haskell-dependencies AS build-artifacts
USER vscode
COPY --chown=vscode:vscode . /workspaces/gcl-all
WORKDIR                      /workspaces/gcl-all
RUN bash -x build.sh


#
# Production runtime image for gcl users.
#
FROM mcr.microsoft.com/devcontainers/base:ubuntu-22.04 AS gcl
COPY --from=build-artifacts --chown=vscode:vscode /home/vscode/.local/bin/gcl /home/vscode/.local/bin/
COPY --from=build-artifacts --chown=vscode:vscode /home/vscode/*.vsix         /home/vscode/


########################################


#
# Development image for gcl developers.
# Includes a prebuilt gcl binary for testing.
#
FROM prebuilt-haskell-dependencies AS gcl-dev
COPY --from=build-artifacts --chown=vscode:vscode /home/vscode/.local/bin/gcl /home/vscode/.local/bin/
