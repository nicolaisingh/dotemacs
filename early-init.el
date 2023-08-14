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

;; f7f7f7=gray97
;; e5e5e5=gray90
;; dbdbdb=gray86
;; d1d1d1=gray82
(setq my-bg-color "#f7f7f7"
      my-bg-hl1-color "#e5e5e5"
      my-bg-hl2-color "#dbdbdb"
      my-bg-hl3-color "#d1d1d1")

;; tsdh-light's #a0a1a7 is too light for me
(set-face-attribute 'font-lock-comment-face t
                    :foreground "#8c8d91")

(set-face-attribute 'default t
                    :background my-bg-color)

(set-face-attribute 'show-paren-match t
                    :weight 'bold
                    :background my-bg-hl2-color)

(set-face-attribute 'fringe t
                    :background my-bg-color)

(set-face-attribute 'mode-line t
                    :foreground "gray30"
                    :background "slategray2"
                    :box '(:line-width 1 :color "slategray4" :style released-button))

(set-face-attribute 'mode-line-inactive t
                    :foreground "gray60"
                    :background my-bg-hl1-color
                    :box '(:line-width 1 :color "gray60" :style released-button))

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

;; Load a nice font
(setq my-font "Inconsolata-11:weight=normal:width=regular")
(when (bound-and-true-p my-font)
  (set-face-attribute 'default t :font my-font)
  (add-to-list 'default-frame-alist `(font . ,my-font)))

(provide 'early-init)
;;; early-init.el ends here
