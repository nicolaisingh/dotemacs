;;; init-eshell.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package eshell.

;;; Code:

(defun eshell-define-aliases ()
  (eshell/alias "1" "ls -1 $*")
  (eshell/alias "ll" "ls -l $*")
  ;; (eshell/alias "mv" "mv -i $*")
  ;; (eshell/alias "cp" "cp -i $*")
  (eshell/alias "df" "df -h"))

(defun eshell-set-config ()
  (setq-local completion-auto-help t)
  (company-mode -1)

  (eshell-define-aliases)

  (define-key eshell-mode-map (kbd "C-c m l s")
    (lambda ()
      (interactive)
      (setq eshell-list-files-after-cd (not eshell-list-files-after-cd))
      (message "Eshell list files after cd: %S" eshell-list-files-after-cd))))

(add-hook 'eshell-mode-hook #'eshell-set-config)

(provide 'init-eshell)
;;; init-eshell.el ends here
