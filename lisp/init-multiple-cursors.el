;;; init-multiple-cursors.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package multiple-cursors.

;;; Code:

(require 'multiple-cursors)

(global-set-key (kbd "C-c mc e") #'mc/edit-lines)
(global-set-key (kbd "C-c mc C-a") #'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c mc C-e") #'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c mc C-SPC") #'mc/mark-all-in-region)
(global-set-key (kbd "C-c mc C-M-SPC") #'mc/mark-all-in-region-regexp)
(global-set-key (kbd "C-c mc i n") #'mc/insert-numbers)
(global-set-key (kbd "C-c mc i l") #'mc/insert-letters)
(global-set-key (kbd "C-c mc C-n") #'mc/mark-next-like-this)
(global-set-key (kbd "C-c mc C-p") #'mc/mark-previous-like-this)

(define-key mc/keymap (kbd "C-c .") #'mc/mark-next-like-this)
(define-key mc/keymap (kbd "C-c ,") #'mc/mark-previous-like-this)
(define-key mc/keymap (kbd "C-c >") #'mc/skip-to-next-like-this)
(define-key mc/keymap (kbd "C-c <") #'mc/skip-to-previous-like-this)

(defun multiple-cursors-mode-my-custom-keys ()
  ;; Enable enter key while having multiple cursors
  (define-key mc/keymap (kbd "<return>") nil))

(add-hook 'multiple-cursors-mode-hook #'multiple-cursors-mode-my-custom-keys)

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
