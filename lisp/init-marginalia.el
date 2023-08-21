;;; init-marginalia.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package marginalia.

;;; Code:

(require 'marginalia)

(define-key minibuffer-local-map (kbd "C-c m m") #'marginalia-cycle)
(marginalia-mode)

(provide 'init-marginalia)
;;; init-marginalia.el ends here
