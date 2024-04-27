#!/usr/bin/env bash

~/.vim/install.sh

mkdir -p ~/.local/opt/
( cd ~/.local/opt/ && \
  git clone https://github.com/erhlee-bird/jmp && \
  cd jmp && \
  ./scripts/install.sh -p ~/.local -b ~/.bashrc )

cat >> ~/.bashrc <<EOF
alias j=jmp

for config in ~/.dotfiles/bashrc.d/*; do
  . "\${config}"
done
EOF
