#!/usr/bin/env bash

mkdir -p ~/.emacs.d
mkdir -p ~/.emacs.d/autosaves
mkdir -p ~/.emacs.d/eshell

ln -sn $(pwd)/init.el ~/.emacs.d/init.el
ln -sn $(pwd)/early-init.el ~/.emacs.d/early-init.el
ln -sn $(pwd)/custom.el ~/.emacs.d/custom.el
ln -sn $(pwd)/lisp ~/.emacs.d/lisp
ln -sn $(pwd)/tree-sitter ~/.emacs.d/tree-sitter
ln -sn $(pwd)/snippets ~/.emacs.d/snippets
ln -sn $(pwd)/eshell/alias ~/.emacs.d/eshell/alias
