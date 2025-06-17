FROM ghcr.io/lcamel/gcl-language-server-devcontainer:latest

USER vscode

# this layer is for caching build dependencies
WORKDIR /tmp/cache-build-deps
COPY gcl/stack.yaml gcl/stack.yaml.lock gcl/package.yaml ./
RUN /home/vscode/.ghcup/bin/stack build --only-dependencies && rm -rf /tmp/cache-build-deps
# now we have ghc and compiled dependencies cached in ~/.stack

COPY --chown=vscode:vscode . /home/vscode/gcl-all
WORKDIR /home/vscode/gcl-all
RUN bash -x -i build.sh