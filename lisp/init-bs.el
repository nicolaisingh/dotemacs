;;; init-bs.el --- Init configuration.

;;; Commentary:

;; This contains other init configuration for the package bs.

;;; Code:

(require 'bs)

(setq bs-default-configuration "files-and-scratch"
      bs-max-window-height 30
      bs-minimal-buffer-name-column 25)

;; bs-default-sort-name is not working
;; (setq bs-default-sort-name "by filename")
(setq bs--current-sort-function (assoc "by filename" bs-sort-functions))

(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-=") #'bs-cycle-next)
(global-set-key (kbd "C-+") #'bs-cycle-previous)

(add-hook 'bs-mode-hook #'hl-line-mode)

(provide 'init-bs)
;;; init-bs.el ends here
