;;; init-theme.el --- Init customization

;;; Commentary:

;; This contains init customization for my color preferences within Emacs.

;;; Code:

(set-face-attribute 'default t
                    :font "DejaVu Sans Mono-10" :background "white")

(set-face-attribute 'variable-pitch t
                    :family "DejaVu Serif"
                    :height 1.1)

(set-face-attribute 'fringe t
                    :background "gray98")

(set-face-attribute 'tab-bar t
                    :inherit 'default
                    :background "grey98")

(set-face-attribute 'window-divider t
                    :distant-foreground "white"
                    :foreground "white")

(set-face-attribute 'window-divider-first-pixel t
                    :distant-foreground "gray90"
                    :foreground "white")

(set-face-attribute 'window-divider-last-pixel t
                    :distant-foreground "gray90"
                    :foreground "white")

(set-face-attribute 'show-paren-match t
                    :weight 'bold
                    :background "gray90")

(set-face-attribute 'region t
                    :background "lightgoldenrod1")

(set-face-attribute 'highlight t
                    :background "azure2")

(set-face-attribute 'secondary-selection t
                    :background "lightgoldenrod2")

(set-face-attribute 'minibuffer-prompt t
                    :weight 'bold
                    :foreground "black")

(let ((bg-active "azure3")
      (bg-inactive "gray96")
      (bg-inactive-border "gray95")
      (fg-inactive "gray50"))
  (set-face-attribute 'mode-line t
                      :background bg-active
                      :box `(:line-width (1 . 1)
                                         :color ,bg-active
                                         :style released-button))
  (set-face-attribute 'mode-line-inactive t
                      :background bg-inactive
                      :foreground fg-inactive
                      :box `(:line-width (1 . 1)
                                         :color ,bg-inactive-border
                                         :style flat-button))
  (set-face-attribute 'tab-bar-tab t
                      :inherit 'tab-bar
                      :background bg-active
                      :box `(:line-width (1 . 1)
                                         :style released-button))
  (set-face-attribute 'tab-bar-tab-inactive t
                      :inherit 'tab-bar
                      :background bg-inactive
                      :foreground fg-inactive
                      :box `(:line-width (1 . 1)
                                         :style released-button)))

(provide 'init-theme)
;;; init-theme.el ends here
