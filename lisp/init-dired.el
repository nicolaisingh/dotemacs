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

(defun current-directory-find-name-dired (pattern)
  "Call `find-name-dired' using dired's current directory."
  (interactive "sFind-name (filename wildcard): ")
  (find-name-dired (dired-current-directory) pattern))

(defun current-directory-find-grep-dired (regexp)
  "Call `find-grep-dired' using dired's current directory."
  (interactive "sFind-grep (grep regexp): ")
  (find-grep-dired (dired-current-directory) regexp))

(dired-set-listing-switches)

(setq dired-hide-details-hide-symlink-targets nil
      dired-hide-details-hide-information-lines nil)

(define-key dired-mode-map (kbd "C-c .") #'toggle-other-files)
(define-key dired-mode-map (kbd "z") #'dired-up-directory)
(define-key dired-mode-map (kbd "C-c m .") #'toggle-other-files)
(define-key dired-mode-map (kbd "C-c m f") #'current-directory-find-name-dired)
(define-key dired-mode-map (kbd "C-c m g") #'current-directory-find-grep-dired)

(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(provide 'init-dired)
;;; init-dired.el ends here
