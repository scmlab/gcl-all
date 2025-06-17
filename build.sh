#!/bin/bash -i

echo "==== $0 ===="
date
id
pwd
uname -a
env

cd gcl

time stack setup                     # will install ghc

time stack build --only-dependencies # compile all the dependencies

time stack build


