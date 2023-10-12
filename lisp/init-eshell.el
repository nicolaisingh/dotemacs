;;; init-eshell.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package eshell.

;;; Code:

(require 'eshell)

(setq eshell-hist-ignoredups t
      eshell-history-size 10000
      eshell-ls-dired-initial-args '("-h")
      eshell-ls-initial-args '("-h")
      eshell-visual-subcommands '(("git" "log" "diff" "show" "shortlog")))

(add-to-list 'eshell-modules-list 'eshell-rebind)
(add-to-list 'eshell-modules-list 'eshell-xtra)

(defun eshell-config ()
  (setq-local completion-auto-help t)
  (company-mode -1)

  (define-key eshell-mode-map (kbd "C-c l") (lambda ()
                                              (interactive)
                                              (eshell/clear-scrollback))))
(add-hook 'eshell-mode-hook #'eshell-config)

;; eshell-help
(require 'esh-help)
(setup-esh-help-eldoc)

;;eshell-up
(require 'eshell-up)

;; eshell-z
(with-eval-after-load 'eshell
  (require 'eshell-z))

(provide 'init-eshell)
;;; init-eshell.el ends here
