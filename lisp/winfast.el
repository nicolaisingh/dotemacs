;;; winfast.el --- Fast window management -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides winfast-mode, a minor mode for fast window management.

;;; Code:

(defvar winfast--fullscreen-p nil)
(defvar winfast--last-window-configuration nil)
(defvar winfast--mode-line-color nil)
(defvar winfast--mode-line-box-style nil)
(defvar winfast--winfast-mode-line-color "khaki")
(defvar winfast--winfast-mode-line-box-style
  `(:line-width 1 :color ,winfast--winfast-mode-line-color :style released-button))

(defvar winfast-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<remap> <winfast-mode>" #'other-window)
    (keymap-set map "M-o" #'other-window)
    (keymap-set map "M-0" #'delete-window)
    (keymap-set map "M-1" #'delete-other-windows)
    (keymap-set map "M-2" #'split-window-below)
    (keymap-set map "M-3" #'split-window-right)
    (keymap-set map "M-m" #'minimize-window)
    (keymap-set map "M-k" #'kill-current-buffer)
    (keymap-set map "M-M" #'maximize-window)
    (keymap-set map "M-=" #'balance-windows)
    (keymap-set map "M-P" #'previous-buffer)
    (keymap-set map "M-N" #'next-buffer)

    (keymap-set map "M-f" #'winfast-toggle-fullscreen-window)
    (keymap-set map "M-s" #'winfast-swap-window-with-other)
    (keymap-set map "M-r" #'winfast-swap-window-with-other-reverse)
    (keymap-set map "M-<return>" #'winfast-swap-window-with-largest)

    (keymap-set map "M-]" #'winfast-enlarge-window)
    (keymap-set map "M-[" #'winfast-shrink-window)
    (keymap-set map "M-}" #'winfast-enlarge-window-horizontally)
    (keymap-set map "M-{" #'winfast-shrink-window-horizontally)
    map)
  "Keymap for `winfast-mode'.
Any other key binding used which is not in the map will turn off
`winfast-mode'.")

(defun winfast-swap-window-with-other (count)
  "Exchange the selected window with the next one COUNT times."
  (interactive "p")
  (let* ((original-window (selected-window))
         (current-buffer (window-buffer (selected-window))))
    (other-window count)
    (set-window-buffer original-window (window-buffer (selected-window)))
    (set-window-buffer (selected-window) current-buffer)))

(defun winfast-swap-window-with-other-reverse (count)
  "Exchange the selected window with the previous one COUNT times."
  (interactive "p")
  (let* ((original-window (selected-window))
         (current-buffer (window-buffer (selected-window))))
    (other-window (* -1 count))
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
         (winfast-command (keymap-lookup winfast-mode-map (key-description key) nil))
         (winfast-command-p (or (commandp winfast-command)
                                (equal key (where-is-internal #'winfast-mode global-map t)))))

    ;; Reset the fullscreen flag if the key is a winfast command
    ;; that is not `winfast-toggle-fullscreen-window'
    (when winfast--fullscreen-p
      (when (and winfast-command-p
                 (not (eq 'winfast-toggle-fullscreen-window winfast-command)))
        (setq winfast--fullscreen-p nil)))

    ;; Turn off winfast-mode if the keybinding does not belong to its
    ;; keymap, and execute the keybinding normally (like isearch).
    (unless winfast-command-p
      (winfast-mode-done))))

(defun winfast-mode-done ()
  "Cleanup before fully turning off `winfast-mode'."
  (remove-hook 'pre-command-hook #'winfast-pre-command-hook)
  (set-face-attribute 'mode-line nil
                      :background winfast--mode-line-color
                      :box winfast--mode-line-box-style)
  (winfast-mode -1))

(defun winfast-enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 10))

(defun winfast-shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 10))

(defun winfast-shrink-window ()
  (interactive)
  (shrink-window 5))

(defun winfast-enlarge-window ()
  (interactive)
  (enlarge-window 5))

(define-minor-mode winfast-mode
  "Minor mode for fast management of Emacs windows."
  :global t
  :group 'winfast
  :init-value nil
  :lighter " WinFast"
  :keymap winfast-mode-map

  (when winfast-mode
    (add-hook 'pre-command-hook #'winfast-pre-command-hook)
    (setq winfast--mode-line-color (face-attribute 'mode-line :background))
    (setq winfast--mode-line-box-style (face-attribute 'mode-line :box))
    (set-face-attribute 'mode-line nil
                        :background winfast--winfast-mode-line-color
                        :box winfast--winfast-mode-line-box-style)))

(provide 'winfast)
;;; winfast.el ends here
