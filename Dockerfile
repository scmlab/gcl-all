FROM ghcr.io/lcamel/gcl-language-server-devcontainer:latest

COPY --chown=vscode:vscode . /home/vscode/gcl-all
USER vscode
WORKDIR /home/vscode/gcl-all

RUN bash -x -i build.sh