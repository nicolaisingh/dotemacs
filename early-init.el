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

(setq my-bg-color "gray97"
      my-mode-line-active-color "slategray2"
      my-mode-line-inactive-color "gray90")

;; tsdh-light's #a0a1a7 is too light for me
(set-face-attribute 'font-lock-comment-face t
                    :foreground "#8c8d91")

(set-face-attribute 'default t
                    :background my-bg-color)

(set-face-attribute 'show-paren-match t
                    :weight 'bold
                    :background "gray85")

(set-face-attribute 'fringe t
                    :background my-bg-color)

(set-face-attribute 'mode-line t
                    :foreground "gray30"
                    :background my-mode-line-active-color
                    :box `(:line-width 2 :color ,my-mode-line-active-color :style released-button))

(set-face-attribute 'mode-line-inactive t
                    :foreground "gray50"
                    :background my-mode-line-inactive-color
                    :box `(:line-width 2 :color ,my-mode-line-inactive-color :style released-button))

(set-face-attribute 'minibuffer-prompt t
                    :foreground "#ffffff"
                    :background "slategray4"
                    :box nil
                    :weight 'normal)

(set-face-attribute 'region t
                    :background "pale goldenrod")

(set-face-attribute 'highlight t
                    :background "azure3")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 180))

(provide 'early-init)
;;; early-init.el ends here
