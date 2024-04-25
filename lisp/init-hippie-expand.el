;;; init-hippie-expand.el --- Init configuration

;;; Commentary:

;; This contains init configuration for hippie-expand.

;;; Code:

;; try-functions for dabbrev expansion
(setq my-he-try-functions-dabbrev
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs))

;; try-functions for expanding lines
(setq my-he-try-functions-line
      '(try-expand-line
        try-expand-line-all-buffers))

;; try-functions for expanding filenames
(setq my-he-try-functions-filename
      '(try-complete-file-name-partially
        try-complete-file-name))

;; Combine all the above as the default behavior for `hippie-expand'
(setq hippie-expand-try-functions-list (append
                                        my-he-try-functions-dabbrev
                                        my-he-try-functions-line
                                        my-he-try-functions-filename))

(fset 'hippie-expand-filename-first
      (make-hippie-expand-function (append
                                    my-he-try-functions-filename
                                    my-he-try-functions-dabbrev
                                    my-he-try-functions-line)
                                   t))

(fset 'hippie-expand-line-first
      (make-hippie-expand-function (append
                                    my-he-try-functions-line
                                    my-he-try-functions-filename
                                    my-he-try-functions-dabbrev)
                                   t))

(defun hippie-expand-disable-minor-modes ()
  (interactive)
  (hippie-expand-line-mode -1)
  (hippie-expand-filename-mode -1))

(define-minor-mode hippie-expand-filename-mode
  "Modify hippie-expand to prioritize filename expansions."
  :global t
  :init-value nil
  :lighter " HippieFilename"
  :keymap
  '(([?\M-/] . hippie-expand-filename-first)
    ([?\C-g] . (lambda ()
                 (interactive)
                 (hippie-expand-disable-minor-modes)
                 (keyboard-quit))))

  (when hippie-expand-filename-mode
    (hippie-expand-line-mode -1)))

(define-minor-mode hippie-expand-line-mode
  "Modify hippie-expand to prioritize line expansions."
  :global t
  :init-value nil
  :lighter " HippieLine"
  :keymap
  '(([?\M-/] . hippie-expand-line-first)
    ([?\C-g] . (lambda ()
                 (interactive)
                 (hippie-expand-disable-minor-modes)
                 (keyboard-quit))))

  (when hippie-expand-line-mode
    (hippie-expand-filename-mode -1)))

(keymap-global-set "M-/" #'hippie-expand)
(keymap-global-set "C-c e f" #'hippie-expand-filename-mode)
(keymap-global-set "C-c e l" #'hippie-expand-line-mode)

(provide 'init-hippie-expand)
;;; init-hippie-expand.el ends here
