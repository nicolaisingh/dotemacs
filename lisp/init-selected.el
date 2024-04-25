;;; init-selected.el --- Init customization

;;; Commentary:

;; This contains init customization for selected.

;;; Code:

(require 'selected)

(let ((map selected-keymap))
  (keymap-set map "C" #'capitalize-region)
  (keymap-set map "F" #'flush-lines)
  (keymap-set map "K" #'keep-lines)
  (keymap-set map "SPC" #'canonically-space-region)
  (keymap-set map "a" #'align-regexp)
  (keymap-set map "f" #'fill-region)
  (keymap-set map "r" #'reverse-region)
  (keymap-set map "s" #'sort-lines)
  (keymap-set map "u" #'unfill-region)
  (keymap-set map "q" #'selected-off))

(defun turn-off-selected-minor-mode ()
  (selected-minor-mode -1))

(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook #'turn-off-selected-minor-mode))

(selected-global-mode 1)

(provide 'init-selected)
;;; init-selected.el ends here
