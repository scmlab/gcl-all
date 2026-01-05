#
# Install Node.js on top of the base image
#
FROM ghcr.io/lcamel/haskell-devcontainer:stackage-lts-24 AS base-with-nodejs
USER vscode

# Install NVM (Node Version Manager) and Node.js 24
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash && \
    export NVM_DIR="$HOME/.nvm" && \
    . "$NVM_DIR/nvm.sh" && \
    echo "nvm version: $(nvm --version)" && \
    nvm install 24 && \
    nvm use 24 && \
    echo '. $NVM_DIR/nvm.sh' >> $HOME/.bashrc


#
# Pre-compile project dependencies to ~/.stack for caching
# This stage seldom changes
#
FROM base-with-nodejs AS builder-deps
USER vscode
WORKDIR /tmp/cache-build-deps
COPY gcl/stack.yaml gcl/stack.yaml.lock gcl/package.yaml ./
RUN . $HOME/.ghcup/env && stack build --only-dependencies && rm -rf /tmp/cache-build-deps


#
# Build the language server and the extension
# The content in /workspaces will be hidden by VS Code's mount
#
FROM builder-deps AS builder
USER vscode
COPY --chown=vscode:vscode . /workspaces/gcl-all
WORKDIR                      /workspaces/gcl-all
RUN bash -x build.sh


#
# Runtime image for the gcl user
#
FROM mcr.microsoft.com/devcontainers/base:ubuntu-22.04 AS gcl-all
COPY --from=builder --chown=vscode:vscode /home/vscode/.local/bin/gcl /home/vscode/.local/bin/
COPY --from=builder --chown=vscode:vscode /home/vscode/*.vsix         /home/vscode/




#
# For gcl-all developers only
# A pre-build gcl binary is helpful for testing
#
FROM builder-deps AS builder-deps-bin
COPY --from=builder --chown=vscode:vscode /home/vscode/.local/bin/gcl /home/vscode/.local/bin/
