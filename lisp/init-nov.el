;;; init-nov.el --- Init customization

;;; Commentary:

;; This contains init customization for the package nov (EPUB reader).

;;; Code:

(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(defun nov-font-setup ()
  (face-remap-add-relative 'default :height 1.1)
  (setq-local nov-text-width 80)
  (nov-render-document))
(add-hook 'nov-mode-hook #'nov-font-setup)

(defun nov-update-text-width (n)
  (interactive (list (or current-prefix-arg
                         (read-number (format "Change nov-text-width from %s to: " nov-text-width)
                                      (current-column)))))
  (setq-local nov-text-width n)
  (nov-render-document))

(keymap-set nov-mode-map "f" #'nov-update-text-width)

(provide 'init-nov)
;;; init-nov.el ends here
