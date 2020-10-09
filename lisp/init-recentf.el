;;; init-recentf.el --- Init customization

;;; Commentary:

;; This contains init customization for the package recentf.

;;; Code:

(add-hook 'after-init-hook #'recentf-mode)

(setq recentf-max-menu-items 100
      recentf-max-saved-items 100
      recentf-menu-filter 'recentf-sort-directories-ascending)

(global-set-key (kbd "C-x C-S-f") #'recentf-open-files)

(provide 'init-recentf)
;;; init-recentf.el ends here
