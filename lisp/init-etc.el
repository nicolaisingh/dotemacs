;;; init-etc.el --- Init configuration.

;;; Commentary:

;; This contains other init configuration not specific to any package.

;;; Code:

(require 'init-etc-modes)

(defun my-find-init-file ()
  "Find my Emacs init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun my-sudo-find-alternate-file ()
  "Find this file as sudo."
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(defun my-scratch-buffer ()
  "Find the *scratch* buffer."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*scratch*")))

(defun my-delete-undo-history ()
  "Delete the undo history of this buffer."
  (interactive)
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun swap-buffer-with-other (count)
  "Swap the current window's buffer with the next window's
buffer, and then select the next window."
  (interactive "p")
  (let* ((original-window (selected-window))
	 (current-buffer (window-buffer (selected-window))))
    (other-window count)
    (set-window-buffer original-window (window-buffer (selected-window)))
    (set-window-buffer (selected-window) current-buffer)))

(defun swap-buffer-with-largest ()
  "Swap the current window's buffer with the largest window's
buffer, and then select the largest window."
  (interactive)
  (let* ((original-window (selected-window))
	 (current-buffer (window-buffer (selected-window))))
    (select-window (get-largest-window))
    (set-window-buffer original-window (window-buffer (selected-window)))
    (set-window-buffer (selected-window) current-buffer)))

(defun my-find-file ()
  (interactive)
  (if current-prefix-arg
      (find-file-in-repository)
    (call-interactively #'find-file)))

(global-set-key (kbd "C-x C-f") #'my-find-file)

(provide 'init-etc)
;;; init-etc.el ends here.
