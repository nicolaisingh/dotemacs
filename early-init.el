;;; early-init.el --- Early init configuration

;;; Commentary:

;; This contains early init configuration, mostly GUI-related.

;;; Code:

(setq inhibit-startup-screen 1)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(setq blink-cursor-interval 0.25)
(setq blink-cursor-blinks 15)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 1)

(column-number-mode 1)

(setq-default cursor-type 'bar)

;; Make the fringe slightly thicker
(add-to-list 'fringe-styles '("padded" 12))
(fringe-mode 12)

(setq my-font "DejaVu Sans Mono-9:weight=book:width=regular")

(load-theme 'tsdh-light)

;; tsdh-light's #a0a1a7 is too light for me
(set-face-attribute 'font-lock-comment-face t
                    :foreground "#8c8d91")

;; (setq my-font "DejaVu Sans Mono-9:weight=book:width=regular")
(set-face-attribute 'default t
                    :font my-font
                    :background "#ffffff")

(set-face-attribute 'show-paren-match t
                    :weight 'bold
                    :background "gray95")

(set-face-attribute 'fringe t
                    :background "#ffffff")

(set-face-attribute 'mode-line t
                    :foreground "gray30"
                    :background "slategray2"
                    :box '(:line-width 1 :color "gray30" :style nil))

(set-face-attribute 'mode-line-inactive t
                    :foreground "gray60"
                    :background "gray96"
                    :box '(:line-width 1 :color "gray60" :style nil))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(add-to-list 'default-frame-alist `(font . ,my-font))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 180))

(provide 'early-init)
;;; early-init.el ends here
