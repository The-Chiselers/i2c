#!/bin/sh

git submodule update --init --recursive
cd nix
nix develop --extra-experimental-features 'nix-command flakes' -c $SHELL
cd ..
