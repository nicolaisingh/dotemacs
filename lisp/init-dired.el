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

(defun ediff-marked-files ()
  "Run ediff-files on 2 marked files in dired.  Inspired by
https://oremacs.com/2017/03/18/dired-ediff"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if (not (= (length files) 2))
        (error "Marked files != 2")
      (ediff-files (first files) (second files)))))

(defun apply-safely (function input)
  "Basically apply, but with safety checks on the function
arity."
  (if (not (fboundp function))
      (error "%s is not a function" function)
    (let* ((arity (func-arity function))
           (min-arity (car arity))
           (max-arity (cdr arity))
           (input-length (length input)))
      (if (not (or (and (numberp max-arity)
                        (>= input-length min-arity)
                        (<= input-length max-arity))
                   (and (>= input-length min-arity)
                        (eq 'many max-arity))))
          (error "Input does not match function arity")
        (apply function input)))))

(defun apply-to-marked-files (function)
  "Prompt for a function and pass the marked dired entries as its
arguments."
  (interactive "CApply to marked dired entries: ")
  (apply-safely function (dired-get-marked-files)))

(dired-set-listing-switches)

(setq dired-hide-details-hide-symlink-targets nil
      dired-hide-details-hide-information-lines nil)

(define-key dired-mode-map (kbd "C-c .") #'toggle-other-files)
(define-key dired-mode-map (kbd "z") #'dired-up-directory)
(define-key dired-mode-map (kbd "C-c m .") #'toggle-other-files)
(define-key dired-mode-map (kbd "C-c m f") #'current-directory-find-name-dired)
(define-key dired-mode-map (kbd "C-c m g") #'current-directory-find-grep-dired)
(define-key dired-mode-map (kbd "C-c m d") #'ediff-marked-files)
(define-key dired-mode-map (kbd "C-c m !") #'apply-to-marked-files)

(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(provide 'init-dired)
;;; init-dired.el ends here
