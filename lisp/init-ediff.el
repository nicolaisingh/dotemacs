;;; init-ediff.el --- Init configuration

;;; Commentary:

;; This contains init configuration for ediff.

;;; Code:

(defvar ediff-temp-buffer-a "*diff-a*")
(defvar ediff-temp-buffer-b "*diff-b*")

(defvar ediff-dired-file-a nil "File A to compare in ediff using ediff-dired-diff")
(defvar ediff-dired-file-b nil "File B to compare in ediff using ediff-dired-diff")

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(defvar ediff-previous-window-config nil
  "Contains the previous window configuration, before ediff was ran.")

(defun ediff-restore-previous-window-config ()
  "Restore the previous window configuration stored in `ediff-previous-window-config'."
  (set-window-configuration ediff-previous-window-config)
  (kill-buffer ediff-temp-buffer-a)
  (kill-buffer ediff-temp-buffer-b))

(defun ediff-save-windows-config ()
  (setq ediff-previous-window-config (current-window-configuration)))

(defun ediff-cleanup-dired-vars ()
  (setq ediff-dired-file-a nil
        ediff-dired-file-b nil)
  (remove-hook 'ediff-quit-hook #'ediff-cleanup-dired-vars))

(add-hook 'ediff-before-setup-hook #'ediff-save-windows-config)
(add-hook 'ediff-quit-hook #'ediff-restore-previous-window-config)

(defun ediff-last-2-kills ()
  "Run ediff on the last 2 kills."
  (interactive)
  (let ((text-a (current-kill 1 t))
        (text-b (current-kill 0 t))
        (a (generate-new-buffer (generate-new-buffer-name ediff-temp-buffer-a)))
        (b (generate-new-buffer (generate-new-buffer-name ediff-temp-buffer-b))))
    (with-current-buffer a (insert text-a))
    (with-current-buffer b (insert text-b))
    (ediff-buffers a b)))

(defun ediff-dired-diff ()
  "Run ediff on 2 files from any dired buffer. The first time this
is called the selected file in dired is treated as file A, and
the file selected during second call will be file B."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (or current-prefix-arg (not ediff-dired-file-a))
        (progn
          (setq ediff-dired-file-a file)
          (message "ediff-dired-diff: File A set"))
      (setq ediff-dired-file-b file)
      (add-hook 'ediff-quit-hook #'ediff-cleanup-dired-vars)
      (ediff-files ediff-dired-file-a ediff-dired-file-b))))

(global-set-key (kbd "C-c d k") #'ediff-last-2-kills)
(global-set-key (kbd "C-c d d") #'ediff-dired-diff)

(provide 'init-ediff)
;;; init-ediff.el ends here
