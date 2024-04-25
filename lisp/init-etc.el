;;; init-etc.el --- Init configuration

;;; Commentary:

;; This contains other init configuration not specific to any package.

;;; Code:

(require 'init-etc-modes)

(defun my-find-init-file ()
  "Find my Emacs init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun sudo-find-file (file-or-buffer)
  "Find a local file/directory as sudo."
  (let ((sudomethod (if (< emacs-major-version 27) "/sudo::" "/sudoedit::")))
    (find-file (concat sudomethod file-or-buffer))))

(defun sudo-find-alternate-file-local (file-or-buffer)
  "Alternate find a local file/directory as sudo."
  (let ((sudomethod (if (< emacs-major-version 27) "/sudo::" "/sudoedit::")))
    (find-alternate-file (concat sudomethod file-or-buffer))))

(defun sudo-find-alternate-file-tramp (tramp-file-name)
  "Alternate find a tramp file/directory as sudo."
  (let ((sudomethod "sudo")
        (sudouser "root")
        (vec (tramp-dissect-file-name tramp-file-name)))
    (find-alternate-file
     (tramp-make-tramp-file-name
      sudomethod
      sudouser
      (tramp-file-name-domain vec)
      (tramp-file-name-host vec)
      nil ;; PORT
      (tramp-file-name-localname vec)
      (tramp-make-tramp-hop-name vec)))))

(defun sudo-find-alternate-file ()
  "Alternate find a file/directory as sudo.

Emacs 27 introduced a connection method `/sudoedit' for security
reasons.  Use this if it is available.  Otherwise, use `/sudo'."
  (interactive)
  (require 'tramp)
  (let ((current-buffer (cond
                         ((derived-mode-p 'dired-mode) (dired-current-directory))
                         ((buffer-file-name) (buffer-file-name))
                         (t (error "Buffer is not visiting a file")))))
    (if (tramp-tramp-file-p current-buffer)
        (sudo-find-alternate-file-tramp current-buffer)
      (sudo-find-alternate-file-local current-buffer))))

(defun scratch-buffer ()
  "Find the *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun my-delete-undo-history ()
  "Delete the undo history of this buffer."
  (interactive)
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun indent-using-tabs-and-fixup ()
  (interactive)
  (setq-local indent-tabs-mode t)
  (tabify (point-min) (point-max)))

(defun indent-using-spaces-and-fixup ()
  (interactive)
  (setq-local indent-tabs-mode nil)
  (untabify (point-min) (point-max)))

(defun indent-tab-width-2 ()
  (interactive)
  (setq-local tab-width 2)
  (message "Tab width set to 2.  C-l to refresh the display."))

(defun indent-tab-width-4 ()
  (interactive)
  (setq-local tab-width 4)
  (message "Tab width set to 4.  C-l to refresh the display."))

(defun indent-tab-width-8 ()
  (interactive)
  (setq-local tab-width 8)
  (message "Tab width set to 8.  C-l to refresh the display."))

(defun indent-spaces ()
  (interactive)
  (setq-local indent-tabs-mode nil))

(defun final-newline ()
  (interactive)
  (setq-local require-final-newline t))

(defun indent-tabs ()
  (interactive)
  (setq-local indent-tabs-mode t))

(defun mark-line (&optional arg)
  (interactive "p")
  (if (and (region-active-p)
           (eq last-command this-command))
      (forward-line (if (> (mark) (point)) -1 1))
    (push-mark)
    (push-mark (beginning-of-line) nil t)
    (forward-line arg)))

(defun toggle-line-and-column-numbers ()
  "Toggle `line-number-mode' and `column-number-mode'."
  (interactive)
  (line-number-mode 'toggle)
  (column-number-mode 'toggle))

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

(defun repeatkey-repeatable-call (f &optional other-bindings)
  "Call function F interactively, then allow the last key used to
  repeat the call, similar to C-x z z z in `repeat'."
  (setq repeatkey-last-command f)
  (let ((repeat-char last-command-event))
    (call-interactively f)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector repeat-char)
                   (lambda ()
                     (interactive)
                     (repeatkey-repeatable-call repeatkey-last-command)))
       (dolist (binding other-bindings)
         (define-key map (kbd (car binding)) (cdr binding)))
       map))))

(defun toggle-hscroll-mode ()
  "Toggle `auto-hscroll-mode' between t and 'current-line."
  (interactive)
  (let ((value (if (eq auto-hscroll-mode t) 'current-line t)))
    (setq auto-hscroll-mode value)
    (message "auto-hscroll-mode: %s" value)))

(defun empty-command ()
  "A command that doesn't do anything."
  (interactive))

(defun profiler-toggle ()
  (interactive)
  (require 'profiler)
  (if (profiler-running-p)
      (profiler-stop)
    (profiler-start 'cpu+mem)))

(add-hook 'find-file-hook #'handle-large-file)

(keymap-global-set "C-c i TAB" #'indent-using-tabs-and-fixup)
(keymap-global-set "C-c i SPC" #'indent-using-spaces-and-fixup)
(keymap-global-set "C-c i 2" #'indent-tab-width-2)
(keymap-global-set "C-c i 4" #'indent-tab-width-4)
(keymap-global-set "C-c i 8" #'indent-tab-width-8)
(keymap-global-set "C-c f #" #'sudo-find-alternate-file)
(keymap-global-set "C-c f s" #'scratch-buffer)
(keymap-global-set "C-c m l" #'mark-line)
(keymap-global-set "C-c l n" #'toggle-line-and-column-numbers)
(keymap-global-set "C-c h n" #'highlight-numbers-mode)
(keymap-global-set "C-c h s" #'toggle-hscroll-mode)
(keymap-global-set "C-c P P" #'profiler-toggle)
(keymap-global-set "C-c P R" #'profiler-report)

(provide 'init-etc)
;;; init-etc.el ends here
