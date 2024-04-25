;;; init-multi-term.el --- Init customization

;;; Commentary:

;; This contains init customization for multi-term.

;;; Code:

(require 'term)
(require 'multi-term)

(setq multi-term-program "/run/current-system/sw/bin/bash")

(keymap-global-set "C-c t T" #'multi-term)
(keymap-global-set "C-c t t" #'multi-term-next)

(provide 'init-multi-term)
;;; init-multi-term.el ends here
