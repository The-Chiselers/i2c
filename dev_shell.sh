#!/bin/sh

git submodule update --init --recursive
cd nix
dos2unix *
nix develop --extra-experimental-features 'nix-command flakes' -c $SHELL
cd ..
