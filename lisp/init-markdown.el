;;; init-markdown.el --- Init customization

;;; Commentary:

;; This contains init customization for markdown.

;;; Code:

(defun markdown-my-config ()
  (indent-tabs-mode -1)
  (virtual-auto-fill-mode 1)
  (setq fill-column 80
        markdown-unordered-list-item-prefix "  * "))

(add-hook 'markdown-mode-hook #'markdown-my-config)

(with-eval-after-load 'markdown-mode
  (let ((map markdown-mode-map))
    (define-key map (kbd "C-<return>") #'markdown-insert-header-dwim)
    (define-key map (kbd "M-<left>") #'markdown-promote)
    (define-key map (kbd "M-<right>") #'markdown-demote)))

(provide 'init-markdown)
;;; init-markdown.el ends here
