;;; init-marginalia.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package marginalia.

;;; Code:

(require 'marginalia)

(keymap-set minibuffer-local-map "C-c m m" #'marginalia-cycle)
(marginalia-mode)

(provide 'init-marginalia)
;;; init-marginalia.el ends here
