#!/usr/bin/env bash

set -euo pipefail

mkdir -p "${HOME}/.vim/bundle"

if [[ ! -d "${HOME}/.vim/bundle/Vundle.vim" ]]; then
  git clone https://github.com/VundleVim/Vundle.vim \
    "${HOME}/.vim/bundle/Vundle.vim"
fi

vim +PluginInstall +qall
