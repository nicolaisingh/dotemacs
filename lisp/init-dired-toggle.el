;;; init-dired-toggle.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package dired-toggle.

;;; Code:

(require 'dired-toggle)

(defun dired-toggle-my-find-file ()
  (interactive)
  (let* ((dired-toggle-enabled (if dired-toggle-mode 1 0))
         (dired-hide-details-enabled (if dired-hide-details-mode 1 0))
         (buffer (current-buffer))
         (file (dired-get-file-for-visit))
         (dir-p (file-directory-p file)))
    (if dir-p
        (find-alternate-file file)
      (let ((dired-toggle-window (selected-window))
            (dired-toggle-buffer (window-buffer (selected-window))))
        (dired-find-file-other-window)
        (delete-window dired-toggle-window)
        (kill-buffer dired-toggle-buffer)))
    (when (eq major-mode 'dired-mode)
      (dired-toggle-mode dired-toggle-enabled)
      (dired-hide-details-mode dired-hide-details-enabled))))

(defun dired-toggle-my-quit ()
  "Quit action under `dired-toggle-mode'.  Kill the buffer
instead of burying it."
  (interactive)
  (if (one-window-p)
      (quit-window 1)
    (kill-buffer-and-window)))

;; There seems to be a bug with dired-toggle-find-file.  There are
;; instances where finding a file displays it in the same window as
;; the dired-toggle window with the same narrow size.
(define-key dired-toggle-mode-map (kbd "RET") #'dired-toggle-my-find-file)
(define-key dired-toggle-mode-map (kbd "q") #'dired-toggle-my-quit)
(define-key dired-toggle-mode-map (kbd "^") #'dired-toggle-up-directory)
(define-key dired-toggle-mode-map (kbd "z") #'dired-toggle-up-directory)

(setq dired-toggle-window-size 32)
(setq dired-toggle-window-side 'left)

(add-hook 'dired-toggle-mode-hook
          (lambda ()
            (interactive)
            (visual-line-mode 1)
            (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
            (setq-local word-wrap nil)))

(global-set-key (kbd "C-c d t") #'dired-toggle)

(provide 'init-dired-toggle)
;;; init-dired-toggle.el ends here
