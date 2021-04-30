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

;; prevnext-buffer-mode

(defun prevnext-buffer-mode-done ()
  "Cleanup before fully turning off prevnext-buffer-mode."
  (remove-hook 'pre-command-hook #'prevnext-buffer-pre-command-hook)
  (prevnext-buffer-mode -1))

(defun prevnext-buffer-pre-command-hook ()
  (let* ((key (this-single-command-keys))
         (prevnext-buffer-key (lookup-key prevnext-buffer-mode-map key nil)))
    (unless (commandp prevnext-buffer-key)
      (prevnext-buffer-mode-done))))

(define-minor-mode prevnext-buffer-mode
  "Minor mode for better previous and next buffer switching (C-x left and C-x right bindings)."
  :global t
  :init-value nil
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [left] #'previous-buffer)
            (define-key map [right] #'next-buffer)
            map)

  (when prevnext-buffer-mode
    (add-hook 'pre-command-hook #'prevnext-buffer-pre-command-hook)))

(defun prevnext-buffer-mode-switch-next-then-start ()
  (interactive)
  (next-buffer)
  (prevnext-buffer-mode 1))

(defun prevnext-buffer-mode-switch-previous-then-start ()
  (interactive)
  (previous-buffer)
  (prevnext-buffer-mode 1))

(global-set-key (kbd "C-x <left>") #'prevnext-buffer-mode-switch-previous-then-start)
(global-set-key (kbd "C-x <right>") #'prevnext-buffer-mode-switch-next-then-start)

(provide 'init-etc-modes)
;;; init-etc-modes.el ends here
