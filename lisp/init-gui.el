;;; init-gui.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the Emacs GUI.

;;; Code:

(setq my-scratch-message ";;; This is the scratch buffer

;;; Key Binding Conventions:

;; For user bindings:	C-c LETTER, <F5>-<F9>
;; For major modes:	C-c <CONTROL-CHAR>, C-c C-#, C-c { } < > : or ;
;; For minor modes:	C-c <any other symbol or punctuation>
;; Don't bind these:	C-h following any prefix char (e.g. C-h C-c) or C-x

;;; dired notes:

;; Open files using external programs:
;; & PROGRAMNAME RET	dired-do-async-shell-command
;; & PROGRAMNAME * RET	dired-do-async-shell-command on marked files as a list
;; ! PROGRAMNAME RET	dired-do-shell-command
;; ! PROGRAMNAME * RET	dired-do-shell-command on marked files as a list
;; C-u s	Specify custom dired listing switches (-S=size; -X=extension)

;; Navigation:
;; i
;; C-M-d	dired-tree-down
;; C-M-u	dired-tree-up
;; C-M-n	dired-next-subdir
;; C-M-p	dired-prev-subdir

;; Tree Visibility:
;; $		dired-hide-subdir
;; M-$		dired-hide-all
;; C-u k	dired-do-kill-lines (Removes a subtree)

;; Marked files:
;; *		Prefix for commands on marked files
;; * .		dired-mark-extension
;; * ! or U	dired-unmark-all-marks

(my-find-init-file)
")

(setq initial-scratch-message my-scratch-message)
(setq initial-major-mode 'fundamental-mode)
(save-place-mode 1)

;; Most of the code here are now in early-init.el to utilize the early
;; init mechanism introduced in Emacs 27
(when (< emacs-major-version 27)
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (when (file-exists-p early-init-file)
      (load early-init-file))))

(provide 'init-gui)
;;; init-gui.el ends here
