;;; init-gui.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the Emacs GUI.

;;; Code:

;; (setq my-scratch-message "#+TITLE: Scratch buffer

;; * Important Key Binding Conventions (from elisp manual)
;; ** Reserved for users to customize
;; *** C-c LETTER
;; *** <F5>-<F9>
;; ** Reserved for major modes
;; *** C-c <CONTROL-CHAR>
;; *** C-c C-#
;; *** C-c { } < > : or ;
;; ** Reserved for minor modes
;; *** C-c any other symbol or punctuation
;; ** Don't bind
;; *** C-h following any prefix char (e.g. C-h C-c)
;; *** C-x as this is for Emacs itself
;; ")
(setq my-scratch-message "# This is the scratch buffer

For icomplete:
----
C-M-i		minibuffer-force-complete

M-v		switch-to-completions

M-n		next-history-element
M-p		previous-history-element
M-r		previous-matching-history-element
M-s		next-matching-history-element\n

Completion:
----
M-v		switch-to-completions

Recursive minibuffer:
----
C-]		abort-recursive-edit

Help:
----
C-h S		info-lookup-symbol
C-h o		describe-symbol
")
(setq initial-scratch-message my-scratch-message)

(setq initial-major-mode 'org-mode)
(setq inhibit-startup-screen 1)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(setq blink-cursor-interval 0.25)
(setq blink-cursor-blinks 15)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 1)
(global-hl-line-mode -1)
(save-place-mode 1)

(show-paren-mode 1)
(column-number-mode 1)

(setq-default cursor-type 'bar)

;; (load-theme 'dichromacy)
(load-theme 'tsdh-light)

;; tsdh-light's #a0a1a7 is too light for me
(set-face-attribute 'font-lock-comment-face t
                    :foreground "#8c8d91")

(setq my-font "DejaVu Sans Mono-9:weight=book:width=regular")
(set-face-attribute 'default t :font my-font)

(set-face-attribute 'show-paren-match t
                    :weight 'bold
                    :background "gray95")

(set-face-attribute 'mode-line t
                    :foreground "gray30"
                    :background "slategray1"
                    :box '(:line-width 1 :color "gray30" :style nil))

(set-face-attribute 'mode-line-inactive t
                    :foreground "gray60"
                    :background "gray96"
                    :box '(:line-width 1 :color "gray60" :style nil))

(add-to-list 'default-frame-alist `(font . ,my-font))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 180))

;; Having a background-color in terminals shows the wrong color
(add-to-list 'window-system-default-frame-alist '(x . ((background-color . "white"))))
(add-to-list 'window-system-default-frame-alist '(ns . ((background-color . "white"))))

(provide 'init-gui)
;;; init-gui.el ends here
