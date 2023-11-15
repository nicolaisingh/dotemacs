;;; init-dired.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package dired.

;;; Code:

(require 'dired-x)
(require 'find-dired)
(require 'grep)

(setq dired-listing-switch-A "a"
      dired-isearch-filenames t
      dired-hide-details-hide-symlink-targets nil
      dired-hide-details-hide-information-lines nil
      dired-dwim-target t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)

;; Don't forget `brew install coreutils' in mac
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))

(defun dired-set-listing-switches ()
  (setq dired-listing-switches
        (concat "--group-directories-first -lhv" dired-listing-switch-A)))

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

(defun current-directory-rgrep-dired (regexp files)
  "Run `rgrep' in dired's current directory."
  (interactive "srgrep regexp: \nsfilename wildcard (blank for `*'): ")
  (let* ((case-fold-search (if current-prefix-arg nil t))
         (files (if (equal files "") "*" files)))
    (when (eq grep-find-template nil)
      (grep-compute-defaults))
    (rgrep regexp files (dired-current-directory) nil)))

(defun current-directory-lgrep-dired (regexp files)
  "Run `lgrep' in dired's current directory."
  (interactive "slgrep regexp: \nsfilename wildcard (blank for `*'): ")
  (let* ((case-fold-search (if current-prefix-arg nil t))
         (files (if (equal files "") "*" files)))
    (when (eq grep-find-template nil)
      (grep-compute-defaults))
    (lgrep regexp files (dired-current-directory) nil)))

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

(defvar dired-ediff-file-a nil "File A to compare in ediff using dired-ediff-a-b")
(defvar dired-ediff-file-b nil "File B to compare in ediff using dired-ediff-a-b")

(defun dired-ediff-a-b ()
  "Run ediff on 2 files from any dired buffer. The first time this
is called the selected file in dired is treated as file A, and
the file selected during second call will be file B."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (or current-prefix-arg (not dired-ediff-file-a))
        (progn
          (setq dired-ediff-file-a file)
          (message "dired-ediff-a-b: File A set"))
      (setq dired-ediff-file-b file)
      (add-hook 'ediff-quit-hook #'dired-ediff-a-b-cleanup)
      (ediff-files dired-ediff-file-a dired-ediff-file-b))))

(defun dired-ediff-a-b-cleanup ()
  (setq dired-ediff-file-a nil
        dired-ediff-file-b nil)
  (remove-hook 'ediff-quit-hook #'dired-ediff-a-b-cleanup))

(dired-set-listing-switches)

(defun dired-mode-my-custom-keys ()
  (define-key dired-mode-map (kbd "C-c .") #'toggle-other-files)
  (define-key dired-mode-map (kbd "z") #'dired-up-directory)
  (define-key dired-mode-map (kbd "C-c m .") #'toggle-other-files)
  (define-key dired-mode-map (kbd "C-c m e") #'dired-create-empty-file)
  (define-key dired-mode-map (kbd "C-c m f") #'current-directory-find-name-dired)
  (define-key dired-mode-map (kbd "C-c m g g") #'current-directory-find-grep-dired)
  (define-key dired-mode-map (kbd "C-c m g r") #'current-directory-rgrep-dired)
  (define-key dired-mode-map (kbd "C-c m g l") #'current-directory-lgrep-dired)
  (define-key dired-mode-map (kbd "C-c m d") #'ediff-marked-files)
  (define-key dired-mode-map (kbd "C-c m !") #'apply-to-marked-files)
  (define-key dired-mode-map (kbd "C-c m k") #'dired-dir-to-kill-ring)
  (define-key dired-mode-map (kbd "<tab>") #'origami-toggle-node)
  (define-key dired-mode-map (kbd "<backtab>") #'origami-toggle-all-nodes)
  (define-key dired-mode-map (kbd "C-c d d") #'dired-ediff-a-b))

(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")))

(add-hook 'dired-mode-hook #'dired-mode-my-custom-keys)
(add-hook 'dired-mode-hook #'hl-line-mode)
;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; dired-recent
(dired-recent-mode 1)

(provide 'init-dired)
;;; init-dired.el ends here
