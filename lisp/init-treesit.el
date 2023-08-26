;;; init-treesit.el --- Init configuration

;; This contains init configuration for the package org.

;;; Code:

(require 'treesit)

;; Language grammar downloads:
;; https://github.com/emacs-tree-sitter/tree-sitter-langs

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")))

(provide 'init-treesit)
;;; init-treesit.el ends here
