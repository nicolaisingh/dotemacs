;;; init-dired.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package dired. 

;;; Code:

(require 'dired-x)

(setq dired-listing-switches "-Alh")

(defun dired-set-listing-switches ()
  (setq dired-listing-switches
	(concat "-lh" dired-listing-switch-A)))

(defun toggle-other-files ()
  (interactive)
  (if (string-empty-p dired-listing-switch-A)
      (setq dired-listing-switch-A "A")
    (setq dired-listing-switch-A ""))
  (dired-sort-other (dired-set-listing-switches)))

(define-key dired-mode-map (kbd "C-c .") #'toggle-other-files)

(provide 'init-dired)
;;; init-dired.el ends here
