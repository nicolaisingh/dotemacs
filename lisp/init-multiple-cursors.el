;;; init-multiple-cursors.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package multiple-cursors.

;;; Code:

(require 'multiple-cursors)

(keymap-global-set "C-c m c e" #'mc/edit-lines)
(keymap-global-set "C-c m c C-a" #'mc/edit-beginnings-of-lines)
(keymap-global-set "C-c m c C-e" #'mc/edit-ends-of-lines)
(keymap-global-set "C-c m c C-SPC" #'mc/mark-all-in-region)
(keymap-global-set "C-c m c C-M-SPC" #'mc/mark-all-in-region-regexp)
(keymap-global-set "C-c m c i n" #'mc/insert-numbers)
(keymap-global-set "C-c m c i l" #'mc/insert-letters)
(keymap-global-set "C-c m c C-n" #'mc/mark-next-like-this)
(keymap-global-set "C-c m c C-p" #'mc/mark-previous-like-this)

(keymap-set mc/keymap "C-c ." #'mc/mark-next-like-this)
(keymap-set mc/keymap "C-c ," #'mc/mark-previous-like-this)
(keymap-set mc/keymap "C-c >" #'mc/skip-to-next-like-this)
(keymap-set mc/keymap "C-c <" #'mc/skip-to-previous-like-this)

(defun multiple-cursors-mode-my-custom-keys ()
  ;; Enable enter key while having multiple cursors
  (keymap-unset mc/keymap "<return>"))

(add-hook 'multiple-cursors-mode-hook #'multiple-cursors-mode-my-custom-keys)

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
