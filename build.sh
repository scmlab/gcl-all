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
#time npm run compile
npx @vscode/vsce package --skip-license --allow-missing-repository
# gcl-vscode-0.0.1.vsix

find ~/.local/bin/ -name gcl -ls
find `pwd` -name "*.vsix" -ls

