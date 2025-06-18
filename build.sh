#!/bin/bash

echo "==== $0 ===="
date
id
pwd
uname -a
env

echo "==== build gcl:"
source $HOME/.ghcup/env
cd gcl
time stack setup
time stack build --only-dependencies
time stack build
time stack install # to ~/.local/bin/gcl

cd ..

echo "==== build gcl-vscode:"
export NVM_DIR="$HOME/.nvm"
source "$NVM_DIR/nvm.sh"
cd gcl-vscode
time npm install
npx @vscode/vsce package --skip-license --allow-missing-repository
cp *.vsix $HOME # to ~/gcl-vscode-0.0.1.vsix


echo "==== build results:"
ls -l $HOME/.local/bin/gcl $HOME/*vsix

