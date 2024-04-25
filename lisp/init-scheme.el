;;; init-scheme.el --- Init configuration

;;; Commentary:

;; This contains init configuration for programming in Scheme.

;;; Code:

(defun scheme-mode-my-custom-keys ()
  (keymap-set scheme-mode-map "M-RET" #'scheme-send-last-sexp)
  (keymap-set scheme-mode-map "C-c m s" #'run-scheme))

(add-hook 'scheme-mode-hook #'scheme-mode-my-custom-keys)

(provide 'init-scheme)
;;; init-scheme.el ends here
