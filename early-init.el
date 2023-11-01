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

(load-theme 'tsdh-light)

;; tsdh-light's #a0a1a7 is too light for me
(set-face-attribute 'font-lock-comment-face t
                    :foreground "#8c8d91")

(set-face-attribute 'default t
                    :background "white")

(set-face-attribute 'show-paren-match t
                    :weight 'bold
                    :background "gray90")

(set-face-attribute 'fringe t
                    :background "#f5f5f5")

(set-face-attribute 'mode-line t
                    :foreground "#383a42"
                    :background "slategray1"
                    :box '(:line-width 2 :color "slategray1" :style released-button))

(set-face-attribute 'mode-line-inactive t
                    :foreground "gray50"
                    :background "gray90"
                    :box '(:line-width 2 :color "gray90" :style released-button))

(set-face-attribute 'region t
                    :background "lightgoldenrod1")

(set-face-attribute 'highlight t
                    :background "khaki1")

(set-face-attribute 'secondary-selection t
                    :background "khaki2")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 180))

(provide 'early-init)
;;; early-init.el ends here
