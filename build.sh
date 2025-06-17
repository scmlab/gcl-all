#!/bin/bash

cd gcl

time stack setup                     # will install ghc

time stack build --only-dependencies # compile all the dependencies

time stack build


