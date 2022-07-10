;;; init-yaml.el --- Init configuration

;;; Commentary:

;; This contains init configuration for working with .yaml files.

;;; Code:
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(provide 'init-yaml)
;;; init-yaml.el ends here
