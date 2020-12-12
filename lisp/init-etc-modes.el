;;; init-etc-modes.el --- Custom modes

;;; Commentary:

;; This contains implementation for custom modes.

;;; Code:

(define-minor-mode reader-mode
  "Make a reader-friendly view by removing screen distractions
and adding margins."
  :init-value nil
  :lighter " Reader"
  :global nil
  :group 'reader

  (let ((enabled (if reader-mode t -1)))
    (writeroom-mode enabled)
    (visual-line-mode enabled)))

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.

From https://www.emacswiki.org/emacs/XModMapMode")

(provide 'init-etc-modes)
;;; init-etc-modes.el ends here
