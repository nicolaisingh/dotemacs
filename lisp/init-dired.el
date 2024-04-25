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
      dired-kill-when-opening-new-dired-buffer t
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

(defun dired-return-number-of-marked-files ()
  "Returns the number of marked files.  Extracted from `dired-number-of-marked-files'."
  (let* ((files (dired-get-marked-files nil nil nil t))
         (nmarked
          (cond ((null (cdr files))
                 0)
                ((and (= (length files) 2)
                      (eq (car files) t))
                 1)
                (t
                 (length files))))
         (size (cl-loop for file in files
                        when (stringp file)
                        sum (file-attribute-size (file-attributes file)))))
    nmarked))

(defun dired-get-marked-files-from-all-buffers ()
  "Returns the number of marked files from all dired buffers."
  (let ((marked-files '()))
    (dolist (buf (mapcar #'cdr dired-buffers))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (not (zerop (dired-return-number-of-marked-files)))
            (setq marked-files (append marked-files (dired-get-marked-files)))))))
    marked-files))

(defun dired-get-marked-directories-from-all-buffers ()
  "Same as `dired-get-marked-files-from-all-buffers' but returns only marked directories."
  (cl-remove-if (lambda (elt) (not (file-directory-p elt)))
                (dired-get-marked-files-from-all-buffers)))

(defun dired-copy-file-to-marked-directories ()
  "Copy file to all marked directories across all dired buffers."
  (interactive)
  (let* ((marked-dirs (dired-get-marked-directories-from-all-buffers))
         (selected (dired-get-filename))
         (count (length marked-dirs))
         (i 0))
    (dolist (dir marked-dirs)
      (setq i (+ 1 i))
      (when (y-or-n-p (format "[%d/%d] Copy %s to %s?" i count selected dir))
        (copy-file selected (concat dir "/") nil)))))

(defun dired-unmark-from-all-buffers ()
  "Unmarks all marked files from all dired buffers."
  (interactive)
  (dolist (buf (mapcar #'cdr dired-buffers))
    (when (buffer-live-p buf)
      (with-current-buffer buf (dired-unmark-all-marks))))
  (message "Removed all marks in all dired buffers"))

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
  (keymap-set dired-mode-map "z" #'dired-up-directory)
  (keymap-set dired-mode-map "C-c m ." #'toggle-other-files)
  (keymap-set dired-mode-map "C-c m e" #'dired-create-empty-file)
  (keymap-set dired-mode-map "C-c m f" #'current-directory-find-name-dired)
  (keymap-set dired-mode-map "C-c m g g" #'current-directory-find-grep-dired)
  (keymap-set dired-mode-map "C-c m g r" #'current-directory-rgrep-dired)
  (keymap-set dired-mode-map "C-c m g l" #'current-directory-lgrep-dired)
  (keymap-set dired-mode-map "C-c m d" #'ediff-marked-files)
  (keymap-set dired-mode-map "C-c m !" #'apply-to-marked-files)
  (keymap-set dired-mode-map "C-c m k" #'dired-dir-to-kill-ring)
  (keymap-set dired-mode-map "<tab>" #'origami-toggle-node)
  (keymap-set dired-mode-map "<backtab>" #'origami-toggle-all-nodes)
  (keymap-set dired-mode-map "C-c d d" #'dired-ediff-a-b)
  (keymap-set dired-mode-map "@ c" #'dired-copy-file-to-marked-directories)
  (keymap-set dired-mode-map "@ u" #'dired-unmark-from-all-buffers)
  (keymap-set dired-mode-map "C-c C-a" #'org-attach-dired-to-subtree))

(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")))

(add-hook 'dired-mode-hook #'dired-mode-my-custom-keys)
(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)

;; dired-recent
(dired-recent-mode 1)

(provide 'init-dired)
;;; init-dired.el ends here
