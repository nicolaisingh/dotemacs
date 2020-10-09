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

;;; icomplete keys:

;; C-M-i	minibuffer-force-complete
;; M-v  	switch-to-completions
;;
;; M-n  	next-history-element
;; M-p  	previous-history-element
;; M-r  	previous-matching-history-element
;; M-s  	next-matching-history-element\n
;; M-v  	switch-to-completions

(my-find-init-file)
")

(setq initial-scratch-message my-scratch-message)
(save-place-mode 1)

;; Most of the code here are now in early-init.el to utilize the early
;; init mechanism introduced in Emacs 27
(when (< emacs-major-version 27)
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (when (file-exists-p early-init-file)
      (load early-init-file))))

(provide 'init-gui)
;;; init-gui.el ends here
