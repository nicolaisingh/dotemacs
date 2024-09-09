;; Bootstrap code from: https://github.com/radian-software/straight.el
;; Slight change: Use a local copy of install.el

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       (or (bound-and-true-p straight-base-dir)
                           user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (find-file-noselect
                          (expand-file-name "lisp/straight/install.el"
                                            user-emacs-directory))
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
