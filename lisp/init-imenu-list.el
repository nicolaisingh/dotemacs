;;; init-imenu-list.el --- Init configuration

;;; Commentary:

;; This contains init configurations for the package imenu-list.

;;; Code:

(require 'imenu-list)

(setq imenu-list-size 0.2)
(setq imenu-list-position 'left)
(setq imenu-list-quit-after-jump t)

(defun imenu-list-toggle-quit-after-jump ()
  "Toggles the flag that quits imenu-list after selecting an item
from the list."
  (interactive)
  (setq imenu-list-quit-after-jump (not imenu-list-quit-after-jump))
  (message "%S" imenu-list-quit-after-jump))

(defun displaying-entry-p ()
  "Returns t if a window is temporarily saved in
`save-selected-window--state'.  This can be used to check if
`imenu-list-display-entry' or `imenu-list-goto-entry' is being
used when jumping."
  (boundp 'save-selected-window--state))

(defun imenu-list-my-quit-window ()
  "Quits the imenu-list window regardless of which window is
currently selected."
  (interactive)
  (let ((imenu-list-window (get-buffer-window imenu-list-buffer-name)))
    (when (and imenu-list-quit-after-jump
               imenu-list-window
               (not (displaying-entry-p)))
      (quit-window nil imenu-list-window))))

(add-hook 'imenu-list-after-jump-hook #'imenu-list-my-quit-window)

(add-hook 'imenu-list-major-mode-hook #'hl-line-mode)

(global-set-key (kbd "C-c i m") #'imenu-list)

(provide 'init-imenu-list)
;;; init-imenu-list.el ends here
