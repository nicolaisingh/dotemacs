#!/usr/bin/env bash

initdir=${1:-$(pwd)}

mkdir -p ~/.emacs.d
mkdir -p ~/.emacs.d/auto-save-files
mkdir -p ~/.emacs.d/eshell
mkdir -p ~/.emacs.d/llm-config

ln -sni $initdir/abbrev-defs.el         ~/.emacs.d/abbrev-defs.el
ln -sni $initdir/early-init.el          ~/.emacs.d/early-init.el
ln -sni $initdir/init.el                ~/.emacs.d/init.el

ln -sni $initdir/eshell/alias           ~/.emacs.d/eshell/alias
ln -sni $initdir/llm-config/opencode.json ~/.emacs.d/llm-config/opencode.json

ln -sni $initdir/lisp                   ~/.emacs.d/lisp
ln -sni $initdir/llm-prompts            ~/.emacs.d/llm-prompts
ln -sni $initdir/llm-config             ~/.emacs.d/llm-config
ln -sni $initdir/org-templates          ~/.emacs.d/org-templates
ln -sni $initdir/packages               ~/.emacs.d/packages
ln -sni $initdir/snippets               ~/.emacs.d/snippets
ln -sni $initdir/templates              ~/.emacs.d/templates
ln -sni $initdir/transient              ~/.emacs.d/transient
ln -sni $initdir/tree-sitter            ~/.emacs.d/tree-sitter

ln -sni $initdir/howm-menu.org          ~/.emacs.d/howm-menu.org
