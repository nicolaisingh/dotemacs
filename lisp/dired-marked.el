;;; dired-marked.el --- Operations on marked dired items

;;; Commentary:

;; Helper functions to work on marked directories or files across
;; multiple Dired buffers.

;;; Code:

(require 'dired)
(require 'cl-lib)

(defun dired-marked-return-number-of-marked-files ()
  "Return the number of marked files.
Extracted from `dired-number-of-marked-files'."
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

(defun dired-marked-get-marked-files-globally ()
  "Return the number of marked files from all Dired buffers."
  (let ((marked-files '()))
    (dolist (buf (mapcar #'cdr dired-buffers))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (not (zerop (dired-marked-return-number-of-marked-files)))
            (setq marked-files (append marked-files (dired-get-marked-files)))))))
    marked-files))

(defun dired-marked-get-marked-dirs-globally ()
  "Like `dired-marked-get-marked-files-globally' but return only marked directories."
  (cl-remove-if (lambda (elt) (not (file-directory-p elt)))
                (dired-marked-get-marked-files-globally)))

(defun dired-marked-copy-file-to-marked-directories ()
  "Copy file to all marked directories across all Dired buffers."
  (interactive)
  (let* ((marked-dirs (dired-marked-get-marked-dirs-globally))
         (selected (dired-get-filename))
         (count (length marked-dirs))
         (i 0))
    (dolist (dir marked-dirs)
      (setq i (+ 1 i))
      (when (y-or-n-p (format "[%d/%d] Copy %s to %s?" i count selected dir))
        (copy-file selected (concat dir "/") nil)))))

(defun dired-marked-unmark-all ()
  "Unmark all marked files from all Dired buffers."
  (interactive)
  (dolist (buf (mapcar #'cdr dired-buffers))
    (when (buffer-live-p buf)
      (with-current-buffer buf (dired-unmark-all-marks))))
  (message "Removed all marks in all dired buffers"))

(provide 'dired-marked)
;;; dired-marked.el ends here
