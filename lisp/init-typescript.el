;;; init-typescript.el --- Init configuration

;;; Commentary:

;; This contains init configuration for programming in Typescript.

;;; Code:

(require 'typescript-ts-mode)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

(setq typescript-ts-mode-indent-offset 2)

(add-hook 'typescript-ts-mode-hook #'smartparens-mode)
(add-hook 'typescript-ts-mode-hook #'smartparens-strict-mode)
(add-hook 'typescript-ts-mode-hook #'show-paren-local-mode)

(provide 'init-typescript)
;;; init-typescript.el ends here
