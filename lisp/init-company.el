;;; init-hippie-expand.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package company.

;;; Code:

(require 'company)
(require 'mode-local)

(setq company-lighter-base "Comp"
      company-minimum-prefix-length 2
      company-idle-delay 0.3
      company-show-quick-access 'left
      company-selection-wrap-around 1
      company-require-match nil
      company-dabbrev-minimum-length 3
      company-dabbrev-downcase nil
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))

(setq-mode-local prog-mode
                 company-minimum-prefix-length 2
                 company-idle-delay 0.1)

(add-to-list 'company-backends 'company-native-complete)
(add-to-list 'company-backends 'company-go)
(add-to-list 'company-backends 'company-restclient)

(with-eval-after-load 'nix-mode
  (add-hook 'nix-mode-hook (lambda ()
                             (interactive)
                             (make-local-variable 'company-backends)
                             (add-to-list 'company-backends 'company-nixos-options))))

(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)
(define-key company-active-map (kbd "C-c C-/") #'company-other-backend)

(global-set-key (kbd "C-M-/") #'company-dabbrev)

(add-hook 'after-init-hook #'global-company-mode)

(provide 'init-company)
;;; init-company.el ends here
