;;; init-multiple-cursors.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package multiple-cursors.

;;; Code:

(require 'multiple-cursors)

;; Enable enter key while having multiple cursors
(define-key mc/keymap (kbd "<return>") nil)

(global-set-key (kbd "C-c mc C-a") #'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c mc C-e") #'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c mc *") #'mc/mark-all-like-this)
(global-set-key (kbd "C-c mc c") #'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c mc r") #'mc/mark-all-in-region)
(global-set-key (kbd "C-c mc e") #'mc/edit-lines)


;; Override the default keys for mmlte (mark-more-like-this-extended)
(defun mc/mmlte--message ()
  (if (eq mc/mark-more-like-this-extended-direction 'up)
      (message "<C-p> to mark previous, <C-n> to mark next, <C-P> to remove, <C-N> to skip")
    (message "<C-n> to mark next, <C-p> to mark previous, <C-N> to remove, <C-P> to skip")))

(define-key mc/mark-more-like-this-extended-keymap (kbd "C-p") #'mc/mmlte--up)
(define-key mc/mark-more-like-this-extended-keymap (kbd "C-n") #'mc/mmlte--down)
(define-key mc/mark-more-like-this-extended-keymap (kbd "C-S-n") #'mc/mmlte--left)
(define-key mc/mark-more-like-this-extended-keymap (kbd "C-S-p") #'mc/mmlte--right)

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
