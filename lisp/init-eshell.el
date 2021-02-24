;;; init-eshell.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package eshell.

;;; Code:

(defun eshell-set-config ()
  (setq-local completion-auto-help t)
  (company-mode -1)

  (define-key eshell-mode-map (kbd "C-c m l s")
    (lambda ()
      (interactive)
      (setq eshell-list-files-after-cd (not eshell-list-files-after-cd))
      (message "Eshell list files after cd: %S" eshell-list-files-after-cd))))

(add-hook 'eshell-mode-hook #'eshell-set-config)

(provide 'init-eshell)
;;; init-eshell.el ends here
