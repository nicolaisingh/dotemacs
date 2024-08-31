;;; personal-theme.el --- Personal color theme -*- lexical-binding: t -*-

;;; Commentary:

;; Personal theme colors.

;;; Code:

(deftheme personal "Personal theme." :background-mode 'light)

(let* ((bg-active "gray75")
       (bg-active-border "gray60")
       (bg-inactive "gray95")
       (bg-inactive-border "gray80")
       (fg-inactive "gray50")
       (box-active `(:line-width 1 :color ,bg-active-border :style released-button))
       (box-inactive `(:line-width 1 :color ,bg-inactive-border :style flat-button)))

  (custom-theme-set-faces
   'personal

   '(default ((t (:font "Noto Sans Mono-10" :background "white"))))
   '(variable-pitch ((t (:family "Noto Serif" :height 1.1))))
   `(cursor ((t (:background "tomato"))))
   `(minibuffer-prompt ((t (:weight bold))))

   '(highlight ((t (:background "azure2"))))
   '(region ((t (:background "lightgoldenrod1"))))
   '(secondary-selection ((t (:background "lightgoldenrod2"))))
   '(show-paren-match ((t (:weight bold :background "gray90"))))

   `(mode-line ((t (:background ,bg-active :box ,box-active))))
   `(mode-line-inactive ((t (:background ,bg-inactive :box ,box-inactive :foreground ,fg-inactive))))

   '(tab-bar ((t (:inherit default :background "grey98"))))
   `(tab-bar-tab ((t (:inherit tab-bar :background ,bg-active :box ,box-active))))
   `(tab-bar-tab-inactive ((t (:inherit tab-bar :background ,bg-inactive :box ,box-inactive :foreground ,fg-inactive))))

   '(fringe ((t (:background "gray98"))))
   '(window-divider ((t (:distant-foreground "gray80" :foreground "white"))))
   '(window-divider-first-pixel ((t (:distant-foreground "white" :foreground "white"))))
   '(window-divider-last-pixel ((t (:distant-foreground "white" :foreground "white"))))

   '(emms-metaplaylist-mode-current-face ((t (:inherit emms-metaplaylist-mode-face :inverse-video t :weight bold))))
   '(emms-metaplaylist-mode-face ((t (:inherit default :weight normal))))
   '(emms-playlist-selected-face ((t (:inherit emms-playlist-track-face :inverse-video t :weight bold))))
   '(emms-playlist-track-face ((t (:inherit default))))))

(provide-theme 'personal)
;;; personal-theme.el ends here
