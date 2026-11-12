#
# Base Haskell image: GHC, stack and HLS installed via ghcup.
# Its GHC must match the resolver in gcl/stack.yaml, since stack is set up
# to use the system GHC and not to install its own.
#
FROM ghcr.io/lcamel/haskell-devcontainer:stackage-lts-23.28 AS haskell-base


#
# Installs NVM and Node.js on top of the base Haskell image.
#
FROM haskell-base AS haskell-and-nodejs
USER vscode
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash && \
    export NVM_DIR="$HOME/.nvm" && \
    . "$NVM_DIR/nvm.sh" && \
    echo "nvm version: $(nvm --version)" && \
    nvm install 24 && \
    nvm use 24 && \
    npm install -g corepack@latest && \
    corepack enable pnpm && \
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
# Fetches the z3 SMT solver, used by gcl at runtime (via sbv).
# Only the single `z3` executable + its MIT LICENSE are extracted.
# Arch is selected from TARGETARCH so the same Dockerfile builds x64 and arm64.
# Uses Ubuntu 24.04 for curl/unzip and glibc compatibility with the downloaded
# binaries. `z3 --version` verifies that the executable can run.
#
FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04 AS z3fetch
# TARGETARCH is auto-populated by BuildKit from the target platform
# (amd64 / arm64) -- declared, not assigned. Requires BuildKit (the default).
ARG TARGETARCH
ARG Z3_VER=4.16.0
RUN case "$TARGETARCH" in \
      amd64) Z3_DIR=z3-${Z3_VER}-x64-glibc-2.39   ;; \
      arm64) Z3_DIR=z3-${Z3_VER}-arm64-glibc-2.38 ;; \
      *) echo "unsupported TARGETARCH: $TARGETARCH" >&2; exit 1 ;; \
    esac && \
    curl -fsSL -o /tmp/z3.zip \
      "https://github.com/Z3Prover/z3/releases/download/z3-${Z3_VER}/${Z3_DIR}.zip" && \
    unzip -j /tmp/z3.zip "${Z3_DIR}/bin/z3" "${Z3_DIR}/LICENSE.txt" -d /z3 && \
    test -f /z3/LICENSE.txt && /z3/z3 --version


#
# Builds ormolu, the Haskell formatter checked by CI.
# There is no official aarch64-linux release binary, so it is built from source.
# 0.8.0.2 is the newest release that builds with GHC 9.8 (0.8.1.0 and later
# need ghc-lib-parser 9.14, i.e. GHC >= 9.10). ghc-lib-parser is pinned to the
# version in the official release binaries; the index state pins the rest.
# Keep ORMOLU_VER in sync with the run-ormolu version in .github/workflows/PR.yml.
# Only the stripped ormolu executable (~75MB) is extracted. CABAL_DIR (~2GB)
# is removed in the same RUN to keep the cabal store out of the layer cache.
#
FROM haskell-base AS ormolu-build
USER vscode
ARG ORMOLU_VER=0.8.0.2
ARG GHC_LIB_PARSER_VER=9.12.1.20250105
ARG HACKAGE_INDEX_STATE=2026-11-01T00:00:00Z
RUN . $HOME/.ghcup/env && \
    export CABAL_DIR=/tmp/cabal && \
    cabal update && \
    cabal install "ormolu-${ORMOLU_VER}" \
      --index-state="${HACKAGE_INDEX_STATE}" \
      --constraint="ghc-lib-parser==${GHC_LIB_PARSER_VER}" \
      --installdir=$HOME/.local/bin --install-method=copy && \
    rm -rf /tmp/cabal && \
    strip $HOME/.local/bin/ormolu && \
    $HOME/.local/bin/ormolu --version


#
# Development image for gcl developers.
# Includes a prebuilt gcl binary for testing.
#
FROM prebuilt-haskell-dependencies AS gcl-dev
COPY --from=build-artifacts --chown=vscode:vscode /home/vscode/.local/bin/gcl /home/vscode/.local/bin/
COPY --from=z3fetch --chown=vscode:vscode /z3/z3          /home/vscode/.local/bin/z3
COPY --from=z3fetch --chown=vscode:vscode /z3/LICENSE.txt /home/vscode/.local/share/z3/LICENSE.txt
COPY --from=ormolu-build --chown=vscode:vscode /home/vscode/.local/bin/ormolu /home/vscode/.local/bin/
