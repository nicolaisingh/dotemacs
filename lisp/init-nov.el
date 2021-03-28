;;; init-nov.el --- Init customization

;;; Commentary:

;; This contains init customization for the package nov (EPUB reader).

;;; Code:

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(defun nov-font-setup ()
  (face-remap-add-relative 'default :height 1.2)
  (face-remap-add-relative 'variable-pitch :family "Serif" :height 1.2)
  (centered-cursor-mode t)
  (setq nov-text-width 70
        line-spacing 0.3
        visual-fill-column-center-text t))
(add-hook 'nov-mode-hook #'nov-font-setup)

(add-hook 'nov-mode-hook #'visual-line-mode)
(add-hook 'nov-mode-hook #'visual-fill-column-mode)

(provide 'init-nov)
;;; init-nov.el ends here
