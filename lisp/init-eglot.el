;;; init-eglot.el --- Init customization

;;; Commentary:

;; This contains init customization for eglot.

;;; Code:

(require 'eglot)

(setq eglot-autoshutdown t)
(setq flymake-no-changes-timeout 1)

(setq-mode-local python-mode eglot-ignored-server-capabilities '(:documentHighlightProvider))

(provide 'init-eglot)
;;; init-eglot.el ends here
