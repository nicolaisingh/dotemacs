;;; howm-attach.el --- Attach files using goto links.

;;; Commentary:

;;; Code:

(require 'howm)

(defvar howm-attach-dir (abbreviate-file-name (concat howm-directory "data/"))
  "Where to store attachments.")

(defvar howm-attach-src-dir (abbreviate-file-name "~/Downloads/")
  "Source dir for attachments.")

(defun howm-attach (file)
  (interactive (list (read-file-name "Attach file: "
                                     howm-attach-src-dir nil t)))
  (let* ((dest-dir (expand-file-name
                    (file-name-sans-extension (buffer-name))
                    howm-attach-dir))
         (dest (expand-file-name
                (file-name-nondirectory file)
                dest-dir)))
    (unless (file-exists-p dest-dir)
      (mkdir dest-dir t))
    (ignore-errors
      (copy-file file dest 1 t))
    (insert howm-ref-header
            " "
            (abbreviate-file-name dest))))

(defun howm-attach-jump-to-dired ()
  "Open the current note's attachment directory in dired."
  (interactive)
  (let* ((dest-dir (expand-file-name
                    (file-name-sans-extension (buffer-name))
                    howm-attach-dir))
         (dest-file (save-excursion
                      (move-beginning-of-line 1)
                      (re-search-forward howm-ref-header (line-end-position) t)
                      (expand-file-name (string-trim
                                         (buffer-substring
                                          (point)
                                          (line-end-position)))))))
    (dired dest-dir)
    (dired-goto-file dest-file)))

(defun howm-attach-jump-to-note ()
  "Search within howm the current file in dired."
  (interactive)
  (if (not (eq major-mode 'dired-mode))
      (message "Not in dired-mode")
    (let* ((file (dired-get-filename)))
      (howm-search (abbreviate-file-name file) nil))))

(provide 'howm-attach)
;;; howm-attach.el ends here
