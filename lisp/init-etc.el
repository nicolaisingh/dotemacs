;;; init-etc.el --- Init configuration.

;;; Commentary:

;; This contains other init configuration not specific to any package.

;;; Code:

(require 'init-etc-modes)

(defun my-find-init-file ()
  "Find my Emacs init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun sudo-find-alternate-file ()
  "Find a file/directory as sudo."
  (interactive)
  (cond
   ((derived-mode-p 'dired-mode) (find-alternate-file (concat "/sudo::" (dired-current-directory))))
   ((unless (buffer-file-name) (error "Buffer is not visiting a file")))
   (t (find-alternate-file (concat "/sudo::" (buffer-file-name))))))

(defun scratch-buffer ()
  "Find the *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun my-delete-undo-history ()
  "Delete the undo history of this buffer."
  (interactive)
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun swap-buffer-with-other (count)
  "Swap the current window's buffer with the next window's
buffer, and then select the next window."
  (interactive "p")
  (let* ((original-window (selected-window))
         (current-buffer (window-buffer (selected-window))))
    (other-window count)
    (set-window-buffer original-window (window-buffer (selected-window)))
    (set-window-buffer (selected-window) current-buffer)))

(defun swap-buffer-with-largest ()
  "Swap the current window's buffer with the largest window's
buffer, and then select the largest window."
  (interactive)
  (let* ((original-window (selected-window))
         (current-buffer (window-buffer (selected-window))))
    (select-window (get-largest-window))
    (set-window-buffer original-window (window-buffer (selected-window)))
    (set-window-buffer (selected-window) current-buffer)))

(defun my-find-file ()
  (interactive)
  (if current-prefix-arg
      (find-file-in-repository)
    (call-interactively #'find-file)))

(defun indent-using-tabs ()
  (interactive)
  (setq-default indent-tabs-mode t)
  (tabify (point-min) (point-max)))

(defun indent-using-spaces ()
  (interactive)
  (setq-default indent-tabs-mode nil)
  (untabify (point-min) (point-max)))

(defun indent-tab-width-4 ()
  (interactive)
  (setq-local tab-width 4))

(defun indent-tab-width-8 ()
  (interactive)
  (setq-local tab-width 8))

(defun mark-line (&optional arg)
  (interactive "p")
  (if (and (region-active-p)
           (eq last-command this-command))
      (forward-line (if (> (mark) (point)) -1 1))
    (push-mark)
    (push-mark (beginning-of-line) nil t)
    (forward-line arg)))

(defun handle-large-file ()
  (interactive)
  (when (> (buffer-size) large-file-warning-threshold)
    (font-lock-mode -1)))

(defun benchmark-this (repetitions)
  "Time the execution of the last sexp or the region if active.
Specify a prefix argument to perform the execution REPETITIONS
times."
  (interactive "p")
  (let ((form (if (use-region-p)
                  (read (concat "(progn "
                                (buffer-substring-no-properties
                                 (region-beginning)
                                 (region-end))
                                ")"))
                (pp-last-sexp))))
    (if (not form)
        (error "No form found")
      (message "Form: %S\nBenchmark: %S"
               form
               (eval `(benchmark-run ,repetitions ,form))))))

(add-hook 'find-file-hook #'handle-large-file)

(global-set-key (kbd "C-x C-f") #'my-find-file)
(global-set-key (kbd "C-c i TAB") #'indent-using-tabs)
(global-set-key (kbd "C-c i SPC") #'indent-using-spaces)
(global-set-key (kbd "C-c i 4") #'indent-tab-width-4)
(global-set-key (kbd "C-c i 8") #'indent-tab-width-8)
(global-set-key (kbd "C-c f #") #'sudo-find-alternate-file)
(global-set-key (kbd "C-c f s") #'scratch-buffer)
(global-set-key (kbd "C-c m l") #'mark-line)

(provide 'init-etc)
;;; init-etc.el ends here.
