;;; howm-attach.el --- Attach files using goto links.

;;; Commentary:

;;; Code:

(require 'howm)

(defvar howm-attach-dir (abbreviate-file-name (concat howm-directory "data/"))
  "Where to store attachments.")

(defun howm-attach (file)
  (interactive (list (read-file-name "Attach file: "
                                     howm-attach-dir nil t)))
  (let* ((dest-dir (expand-file-name
                    (file-name-sans-extension (buffer-name))
                    howm-attach-dir))
         (dest (expand-file-name
                (file-name-nondirectory file)
                dest-dir)))
    (unless (file-exists-p dest-dir)
      (mkdir dest-dir t))
    (ignore-errors
      (copy-file file dest nil t))
    (insert howm-ref-header
            " "
            (abbreviate-file-name dest))))

(provide 'howm-attach)
;;; howm-attach.el ends here
