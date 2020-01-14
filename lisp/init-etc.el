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

(provide 'init-etc)
;;; init-etc.el ends here.
