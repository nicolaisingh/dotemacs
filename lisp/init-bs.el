;;; init-bs.el --- Init configuration.

;;; Commentary:

;; This contains other init configuration for the package bs.

;;; Code:

(require 'bs)

(setq bs-default-configuration "files-and-scratch")

(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-=") #'bs-cycle-next)
(global-set-key (kbd "C-+") #'bs-cycle-previous)

(provide 'init-bs)
;;; init-bs.el ends here

