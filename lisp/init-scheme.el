;;; init-scheme.el --- Init configuration

;;; Commentary:

;; This contains init configuration for programming in Scheme.

;;; Code:

(defun scheme-mode-my-custom-keys ()
  (define-key scheme-mode-map (kbd "M-RET") #'scheme-send-last-sexp)
  (define-key scheme-mode-map (kbd "C-c m s") #'run-scheme))

(add-hook 'scheme-mode-hook #'scheme-mode-my-custom-keys)

(provide 'init-scheme)
;;; init-scheme.el ends here
