#!/usr/bin/env bash

# Install Plug
curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

mkdir -p ~/.cache/nvim/undo
mkdir -p ~/.cache/nvim/backups
