;;; init-etc-windows.el --- Init configuration

;;; Commentary:

;; This contains functions for managing my windows.

;;; Code:

(defun swap-window-with-other (count)
  "Exchange the selected window with the next one."
  (interactive "p")
  (let* ((original-window (selected-window))
         (current-buffer (window-buffer (selected-window))))
    (other-window count)
    (set-window-buffer original-window (window-buffer (selected-window)))
    (set-window-buffer (selected-window) current-buffer)))

(defun swap-window-with-largest ()
  "Exchange the selected window with the frame's largest window."
  (interactive)
  (let* ((original-window (selected-window))
         (current-buffer (window-buffer (selected-window))))
    (select-window (get-largest-window))
    (set-window-buffer original-window (window-buffer (selected-window)))
    (set-window-buffer (selected-window) current-buffer)))

(defun put-buffer-to-recent-window ()
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

(defun put-buffer-to-other-window (count)
  "Puts the current buffer to the next window.

The previous window will show its previous buffer.  Useful when
you expect a new buffer to appear in the next window, but instead
it pops up in the current one, replacing your current buffer."
  (interactive "p")
  (let* ((current-buffer (current-buffer)))
    (previous-buffer)
    (other-window count)
    (set-window-buffer (selected-window) current-buffer)))

(global-set-key (kbd "C-`") (key-binding (kbd "C-x o")))
(global-set-key (kbd "C-~") #'swap-window-with-other)
(global-set-key (kbd "C-M-~") #'swap-window-with-largest)
(global-set-key (kbd "C-c b r") #'put-buffer-to-recent-window)
(global-set-key (kbd "C-c b o") #'put-buffer-to-other-window)

(provide 'init-etc-windows)
;;; init-etc-windows.el ends here
