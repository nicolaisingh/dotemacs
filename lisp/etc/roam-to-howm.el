;;; roam-to-howm.el --- Convert my org-roam files to howm -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun my-org-roam-to-howm (&optional buffer howm-dir)
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        (file-name-base (file-name-base (buffer-file-name buffer)))
        title filetags time-stamp output-file-name output-buffer
        year month day hour minute)
    (goto-char (point-min))
    ;; title
    (save-excursion
      (re-search-forward "^#\\+title: \\(.*\\)$" nil t)
      (setq title (match-string 1)))
    ;; time-stamp
    (string-match (concat "\\`\\([0-9]\\{4\\}\\)" ; year
                          "\\([0-9]\\{2\\}\\)"    ; month
                          "\\([0-9]\\{2\\}\\)"    ; day
                          "\\([0-9]\\{2\\}\\)"    ; hour
                          "\\([0-9]\\{2\\}\\)")   ; minute
                  file-name-base)
    (setq year (match-string 1 file-name-base)
          month (match-string 2 file-name-base)
          day (match-string 3 file-name-base)
          hour (match-string 4 file-name-base)
          minute (match-string 5 file-name-base)
          time-stamp (format "[%s %s]"
                             (string-join (list year month day) "-")
                             (string-join (list hour minute) ":"))
          output-file-name (format "%s.org" (string-join (list year month day) "-")))
    ;; filetags
    (save-excursion
      (when (re-search-forward "^#\\+filetags: \\(.*\\)$" nil t)
        (setq filetags (let ((match (match-string-no-properties 1)))
                         (if match (string-split match ":" t) nil)))))
    ;; body
    (save-excursion
      (forward-char 1)
      (while (not (bolp))
        (re-search-forward "^\\(?:[^#\\+]\\|[^:]\\)" nil t))
      (kill-ring-save (point) (point-max)))

    (setq output-buffer
          (let ((howm-note (concat
                            (or howm-dir howm-directory "~/howm/")
                            year "/"
                            month "/"
                            output-file-name)))
            (find-file howm-note)))

    (with-current-buffer output-buffer
      (goto-char (point-min))
      (insert "* " title)
      (newline 1)
      (insert time-stamp)
      (newline 2)
      (yank)
      (newline 1)
      (when filetags
        (insert "<<< ")
        (insert (string-join filetags " <<< ")))
      (newline 2))
    (switch-to-buffer output-buffer)))

(provide 'org-roam-to-howm)
;;; roam-to-howm.el ends here
