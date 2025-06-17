FROM ghcr.io/lcamel/gcl-language-server-devcontainer:latest

# $HOME is set to /home/vscode by default in the base image
COPY --chown=vscode:vscode . $HOME/gcl-all
USER vscode
WORKDIR $HOME/gcl-all

RUN bash -x build.sh