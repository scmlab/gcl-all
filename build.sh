#!/bin/bash

echo "==== $0 ===="
date
id
pwd
uname -a
env

source $HOME/.ghcup/env

export NVM_DIR="$HOME/.nvm"
source "$NVM_DIR/nvm.sh"

cd gcl

time stack setup
time stack build --only-dependencies
time stack build
time stack install # to ~/.local/bin/gcl

cd ..

cd gcl-vscode

time npm install
time npm run compile

