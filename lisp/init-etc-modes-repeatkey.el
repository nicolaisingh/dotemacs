;;; init-etc-modes-repeatkey.el --- Minor mode to repeat commands

;;; Commentary:

;; This contains implementation for the minor mode `repeatkey' which
;; allows the user to use the last character in a keybinding to repeat
;; the command, like C-x o o o to repeatedly jump around windows, as
;; long as it is defined properly in this mode's keymap.
;;
;; There is a similar library called `repeat' that has a similar
;; behavior.

;;; Code:

(defvar repeatkey-load-map-p nil
  "Whether the temporary keymap should be loaded or not for the
current command.")

(defun repeatkey-update-map (keymap)
  (setq repeatkey-mode-map keymap)
  (setcdr (assq 'repeatkey-mode minor-mode-map-alist) keymap))

(defun repeatkey-keymap ()
  "Keymap to load which has the repeating keys under `repeatkey-minor-mode.'"
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<left>" #'previous-buffer)
    (keymap-set map "<right>" #'next-buffer)
    (keymap-set map "o" #'other-window)
    (keymap-set map "{" #'shrink-window-horizontally)
    (keymap-set map "}" #'enlarge-window-horizontally)
    (keymap-set map "^" #'enlarge-window)
    (keymap-set map "6" #'shrink-window)
    map))

(defun repeatkey-pre-command-hook ()
  (let* ((mapped-p (where-is-internal this-command `(,(repeatkey-keymap)) t)))
    (if mapped-p
        (setq repeatkey-load-map-p t)
      (setq repeatkey-load-map-p nil)
      (repeatkey-update-map nil))))

(defun repeatkey-post-command-hook ()
  (when repeatkey-load-map-p
    (repeatkey-update-map (repeatkey-keymap))))

(define-minor-mode repeatkey-mode
  "Minor mode for better previous and next buffer switching (C-x left and C-x right bindings)."
  :global t
  :init-value nil
  :lighter nil
  :keymap (make-sparse-keymap)

  (when repeatkey-mode
    (add-hook 'pre-command-hook #'repeatkey-pre-command-hook)
    (add-hook 'post-command-hook #'repeatkey-post-command-hook))
  (unless repeatkey-mode
    (remove-hook 'pre-command-hook #'repeatkey-pre-command-hook)
    (remove-hook 'post-command-hook #'repeatkey-post-command-hook)))

;; Enable this minor mode
(repeatkey-mode t)

(provide 'init-etc-modes-repeatkey)
;;; init-etc-modes-repeatkey.el ends here
