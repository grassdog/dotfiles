#!/usr/bin/env bash


if [[ ! -f ~/.config/nvim/autoload/plug.vim ]]; then
  echo "==> Installing Plug"
  curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

echo "==> Creating backup and undo cache directories"
mkdir -p ~/.cache/nvim/undo
mkdir -p ~/.cache/nvim/backups

echo "==> Installing plugins"
nvim --headless +PlugInstall +qall
