;;; init-diff-hl.el --- Init customization

;;; Commentary:

;; This contains init customization for diff-hl.

;;; Code:

(require 'diff-hl)

(global-diff-hl-mode 1)

(keymap-global-set "C-c d h h" #'global-diff-hl-mode)

(let ((map diff-hl-mode-map))
  (keymap-set map "C-c d h a" #'diff-hl-amend-mode)
  (keymap-set map "C-c d h f" #'diff-hl-flydiff-mode))

(with-eval-after-load 'init-magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(provide 'init-diff-hl)
;;; init-diff-hl.el ends here
