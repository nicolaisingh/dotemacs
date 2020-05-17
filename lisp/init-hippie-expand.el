;;; init-hippie-expand.el --- Init configuration for hippie-expand.

;;; Commentary:

;; This contains init configuration for hippie-expand.

;;; Code:

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(provide 'init-hippie-expand)
;;; init-hippie-expand.el ends here
