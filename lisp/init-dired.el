;;; init-dired.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package dired.

;;; Code:

(require 'dired-x)
(require 'find-dired)

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
  (let* ((case-fold-search nil)
         (find-name-arg
          (if (or current-prefix-arg
                  (string-match-p "[[:upper:]]" pattern))
              "-name"
            "-iname")))
    (find-name-dired (dired-current-directory) pattern)))

(defun current-directory-find-grep-dired (regexp)
  "Call `find-grep-dired' using dired's current directory."
  (interactive "sFind-grep (grep regexp): ")
  (let* ((case-fold-search nil)
         (grep-ignore-case-flag
          (if (or current-prefix-arg
                  (string-match-p "[[:upper:]]" regexp))
              ""
            " -i"))
         (find-grep-options (concat find-grep-options grep-ignore-case-flag)))
    (find-grep-dired (dired-current-directory) regexp)))

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

(defun dired-dir-to-kill-ring ()
  "Insert the dired buffer's current directory to the kill
ring."
  (interactive)
  (kill-new dired-directory)
  (let ((message-log-max nil))
    (message "Current directory inserted to the kill ring (%s)" dired-directory)))

(dired-set-listing-switches)

(setq dired-hide-details-hide-symlink-targets nil
      dired-hide-details-hide-information-lines nil)

(defun dired-mode-my-custom-keys ()
  (define-key dired-mode-map (kbd "C-c .") #'toggle-other-files)
  (define-key dired-mode-map (kbd "z") #'dired-up-directory)
  (define-key dired-mode-map (kbd "C-c m .") #'toggle-other-files)
  (define-key dired-mode-map (kbd "C-c m e") #'dired-create-empty-file)
  (define-key dired-mode-map (kbd "C-c m f") #'current-directory-find-name-dired)
  (define-key dired-mode-map (kbd "C-c m g") #'current-directory-find-grep-dired)
  (define-key dired-mode-map (kbd "C-c m d") #'ediff-marked-files)
  (define-key dired-mode-map (kbd "C-c m !") #'apply-to-marked-files)
  (define-key dired-mode-map (kbd "C-c m k") #'dired-dir-to-kill-ring))

(add-hook 'dired-mode-hook #'dired-mode-my-custom-keys)
(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(provide 'init-dired)
;;; init-dired.el ends here
