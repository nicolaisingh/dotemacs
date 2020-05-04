;;; init-dired.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package dired.

;;; Code:

(require 'dired-x)

(setq dired-listing-switch-A "")

(defun dired-set-listing-switches ()
  (setq dired-listing-switches
	(concat "--group-directories-first -lh" dired-listing-switch-A)))

(defun message-other-files-state ()
  (message "%s other files"
	   (if (string-empty-p dired-listing-switch-A)
	       "Hide"
	     "Show")))

(defun toggle-other-files ()
  (interactive)
  (if (string-empty-p dired-listing-switch-A)
      (setq dired-listing-switch-A "A")
    (setq dired-listing-switch-A ""))
  (message-other-files-state)
  (dired-sort-other (dired-set-listing-switches)))

(dired-set-listing-switches)
(define-key dired-mode-map (kbd "C-c .") #'toggle-other-files)

(provide 'init-dired)
;;; init-dired.el ends here
