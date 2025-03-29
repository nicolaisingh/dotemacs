#!/usr/bin/env bash

initdir=${1:-$(pwd)}

mkdir -p ~/.emacs.d
mkdir -p ~/.emacs.d/autosaves
mkdir -p ~/.emacs.d/eshell

ln -sn $initdir/abbrev-defs.el ~/.emacs.d/abbrev-defs.el
ln -sn $initdir/custom.el ~/.emacs.d/custom.el
ln -sn $initdir/early-init.el ~/.emacs.d/early-init.el
ln -sn $initdir/init.el ~/.emacs.d/init.el
ln -sn $initdir/packages.el ~/.emacs.d/packages.el

ln -sn $initdir/eshell/alias ~/.emacs.d/eshell/alias

ln -sn $initdir/lisp ~/.emacs.d/lisp
ln -sn $initdir/org-templates ~/.emacs.d/org-templates
ln -sn $initdir/snippets ~/.emacs.d/snippets
ln -sn $initdir/tree-sitter ~/.emacs.d/tree-sitter

ln -sn $initdir/howm-menu.org ~/.emacs.d/howm-menu.org
