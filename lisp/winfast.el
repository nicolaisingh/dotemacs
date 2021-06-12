;;; winfast.el --- Fast window management -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides winfast-mode, a minor mode for fast window management.

;;; Code:

(defvar winfast--fullscreen-p nil)
(defvar winfast--last-window-configuration nil)

(defun winfast-swap-window-with-other (count)
  "Exchange the selected window with the next one."
  (interactive "p")
  (let* ((original-window (selected-window))
         (current-buffer (window-buffer (selected-window))))
    (other-window count)
    (set-window-buffer original-window (window-buffer (selected-window)))
    (set-window-buffer (selected-window) current-buffer)))

(defun winfast-swap-window-with-largest ()
  "Exchange the selected window with the frame's largest window.

If the selected window is already the largest, exchange it
instead with the most recently used window."
  (interactive)
  (let* ((current-window (selected-window))
         (current-buffer (window-buffer current-window))
         (largest-window (get-largest-window))
         (mru-window (get-mru-window nil nil t))
         (swap-current-with-largest
          (lambda ()
            (set-window-buffer current-window (window-buffer largest-window))
            (set-window-buffer largest-window current-buffer)
            (select-window largest-window)))
         (swap-largest-with-recent
          (lambda()
            (set-window-buffer current-window (window-buffer mru-window))
            (set-window-buffer mru-window current-buffer)
            (select-window largest-window))))

    (funcall (if (eq current-window largest-window)
                 swap-largest-with-recent
               swap-current-with-largest))
    (recenter)))

(defun winfast-put-buffer-to-recent-window ()
  "Puts the current buffer to the most recent window selected.

Similar to `put-buffer-to-next-window', but puts it instead on
the most recent one.  Useful when you expect a new buffer to
appear in the current window, but instead it pops up in the next
one."
  (interactive)
  (let* ((current-buffer (window-buffer (selected-window)))
         (recent-window (get-mru-window nil nil t)))
    (when recent-window
      (previous-buffer)
      (set-window-buffer recent-window current-buffer)
      (select-window recent-window))))

(defun winfast-put-buffer-to-other-window (count)
  "Puts the current buffer to the next window.

The previous window will show its previous buffer.  Useful when
you expect a new buffer to appear in the next window, but instead
it pops up in the current one, replacing your current buffer."
  (interactive "p")
  (let* ((current-buffer (current-buffer)))
    (previous-buffer)
    (other-window count)
    (set-window-buffer (selected-window) current-buffer)))

(defun winfast-fullscreen-window-layout ()
  "Delete the other windows to make the current window fullscreen.

This saves the current window configuration before deleting the
other windows, so it can be restored using
`winfast-revert-window-layout'."
  (setq winfast--last-window-configuration (current-window-configuration)
        winfast--fullscreen-p t)
  (delete-other-windows))

(defun winfast-revert-window-layout ()
  "Revert to the saved window configuration."
  (set-window-configuration winfast--last-window-configuration)
  (setq winfast--last-window-configuration (current-window-configuration)
        winfast--fullscreen-p nil))

(defun winfast-toggle-fullscreen-window ()
  "Toggle fullscreen mode for the currently focused window.

Revert to the previous window configuration when this command is
called again."
  (interactive)
  (if winfast--fullscreen-p
      (winfast-revert-window-layout)
    (winfast-fullscreen-window-layout)))

(defun winfast-pre-command-hook ()
  (let* ((key (this-single-command-keys))
         (winfast-key (lookup-key winfast-mode-map key nil)))
    ;; Reset the fullscreen flag if the key is a winfast command
    ;; that is not `winfast-toggle-fullscreen-window'
    (when winfast--fullscreen-p
      (when (and (commandp winfast-key)
                 (not (eq 'winfast-toggle-fullscreen-window winfast-key)))
        (setq winfast--fullscreen-p nil)))

    ;; Turn off winfast-mode if the keybinding does not belong to its
    ;; keymap, and execute the keybinding normally (like isearch).
    (unless (commandp winfast-key)
      (winfast-mode-done))))

(defun winfast-mode-done ()
  "Cleanup before fully turning off winfast-mode."
  (remove-hook 'pre-command-hook #'winfast-pre-command-hook)
  (winfast-mode -1))

(defvar winfast-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-o" #'other-window)
    (define-key map "\M-0" #'delete-window)
    (define-key map "\M-1" #'delete-other-windows)
    (define-key map "\M-2" #'split-window-below)
    (define-key map "\M-3" #'split-window-right)
    (define-key map "\M-m" #'minimize-window)
    (define-key map "\M-k" #'kill-buffer-and-window)
    (define-key map "\M-M" #'maximize-window)
    (define-key map "\M-=" #'balance-windows)
    (define-key map "\M-P" #'previous-buffer)
    (define-key map "\M-N" #'next-buffer)

    (define-key map "\M-f" #'winfast-toggle-fullscreen-window)
    (define-key map "\M-s" #'winfast-swap-window-with-other)
    (define-key map [M-return] #'winfast-swap-window-with-largest)
    (define-key map "\M-\r" #'winfast-swap-window-with-largest)

    (define-key map "br" #'winfast-put-buffer-to-recent-window)
    (define-key map "bo" #'winfast-put-buffer-to-other-window)
    map)
  "Keymap for `winfast-mode'.
Any other key binding used which is not in the map will turn off
`winfast-mode'.")

(define-minor-mode winfast-mode
  "Minor mode for fast management of Emacs windows."
  :global t
  :init-value nil
  :lighter " WinFast"
  :keymap winfast-mode-map

  (when winfast-mode
    (add-hook 'pre-command-hook #'winfast-pre-command-hook)))

(provide 'winfast)
;;; winfast.el ends here
