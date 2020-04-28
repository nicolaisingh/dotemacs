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
(setq my-scratch-message "# This is the scratch buffer\n")
(setq initial-scratch-message my-scratch-message)

(setq initial-major-mode 'org-mode)
(setq inhibit-startup-screen 1)
(setq visible-bell 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
;; (global-hl-line-mode 1)

(show-paren-mode 1)
(column-number-mode 1)

(setq-default cursor-type 'bar)

(load-theme 'dichromacy)

(setq my-font "DejaVu Sans Mono-9:weight=book:width=regular")
(set-face-attribute 'default t :font my-font)

(add-to-list 'default-frame-alist `(font . ,my-font))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 180))
(add-to-list 'default-frame-alist '(background-color . "white smoke"))

(provide 'init-gui)
;;; init-gui.el ends here
