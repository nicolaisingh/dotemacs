;;; personal-theme.el --- Personal color theme -*- lexical-binding: t -*-

;;; Commentary:

;; Personal theme colors.

;;; Code:

(deftheme personal "Personal theme." :background-mode 'light)

(let*
    (;; Use colors the same as `dichromacy'
     (color-orange "#e69f00")
     (color-sky-blue "#56b4e9")         ; cyan
     (color-bluish-green "#009e73")     ; green
     (color-yellow "#f0e442")           ; yellow
     (color-blue "#0072b2")             ; blue
     (color-vermillion "#d55e00")       ; red
     (color-reddish-purple "#cc79a7")   ; magenta

     (color-orange-dark "#b37c00")
     (color-sky-blue-dark "#438db6")
     (color-bluish-green-dark "#006b4e")
     (color-yellow-dark "#bdb434")
     (color-blue-dark "#00517f")
     (color-vermillion-dark "#a24700")
     (color-reddish-purple-dark "#995b7d")

     (color-sky-blue-light "#5ec5ff")

     ;; frame elements
     (bg-active "gray95")
     (bg-active-border color-reddish-purple)
     (bg-inactive "gray95")
     (bg-inactive-border "gray90")
     (bg-active-tab "white")
     (bg-inactive-tab "gray90")
     (fg-inactive "gray50")
     (box-active `(:line-width 2 :color ,bg-active-border :style flat-button))
     (box-inactive `(:line-width 2 :color ,bg-inactive-border :style flat-button))
     (box-active-tab `(:line-width 1 :color ,bg-active-tab :style released-button))
     (box-inactive-tab `(:line-width 1 :color ,bg-inactive-tab :style released-button)))

  (custom-theme-set-faces
   'personal

   `(default ((t (:font "Noto Sans Mono-10" :foreground "gray10" :background "white"))))
   `(variable-pitch ((t (:family "Noto Serif" :height 1.1))))
   `(cursor ((t (:background "black"))))
   `(minibuffer-prompt ((t (:weight bold))))

   `(highlight ((t (:background "gray95"))))
   `(region ((t (:background ,color-yellow))))
   `(secondary-selection ((t (:background ,color-yellow-dark))))
   `(show-paren-match ((t (:weight bold :background "gray90"))))
   `(isearch ((t (:foreground "white" :background ,color-reddish-purple))))
   `(lazy-highlight ((t (:distant-foreground "black" :background ,color-sky-blue-light))))

   `(mode-line ((t (:background ,bg-active :box ,box-active))))
   `(mode-line-inactive ((t (:background ,bg-inactive :box ,box-inactive :foreground ,fg-inactive))))
   `(mode-line-buffer-id ((t (:weight bold))))

   `(tab-bar ((t (:inherit default :background "gray97"))))
   `(tab-bar-tab ((t (:inherit tab-bar :background ,bg-active-tab :box ,box-active-tab))))
   `(tab-bar-tab-inactive ((t (:inherit tab-bar :background ,bg-inactive-tab :box ,box-inactive-tab :foreground ,fg-inactive))))

   `(fringe ((t (:background "gray98"))))
   `(window-divider ((t (:distant-foreground "gray80" :foreground "white"))))
   `(window-divider-first-pixel ((t (:distant-foreground "white" :foreground "white"))))
   `(window-divider-last-pixel ((t (:distant-foreground "white" :foreground "white"))))

   `(diff-hl-change ((t (:background "aliceblue" :foreground ,color-blue))))
   `(diff-added ((t (:inherit diff-changed :extend t :background "honeydew" :foreground ,color-bluish-green))))
   `(diff-removed ((t (:inherit diff-changed :extend t :background "mistyrose" :foreground ,color-vermillion))))

   `(emms-metaplaylist-mode-face ((t (:inherit default :weight normal))))
   `(emms-metaplaylist-mode-current-face ((t (:inherit emms-metaplaylist-mode-face :inverse-video t :weight bold))))
   `(emms-playlist-track-face ((t (:inherit default))))
   `(emms-playlist-selected-face ((t (:inherit emms-playlist-track-face :inverse-video t :weight bold))))

   `(error ((t (:weight bold :foreground ,color-vermillion))))
   `(warning ((t (:weight bold :foreground ,color-orange))))
   `(success ((t (:weight bold :foreground ,color-bluish-green))))
   `(link ((t (:foreground ,color-blue :underline t))))
   `(link-visited ((t (:inherit link :foreground ,color-reddish-purple))))
   `(font-lock-builtin-face ((t (:foreground ,color-blue))))
   `(font-lock-comment-face ((t (:foreground ,color-orange-dark))))
   `(font-lock-constant-face ((t (:foreground ,color-sky-blue))))
   `(font-lock-function-name-face ((t (:foreground ,color-blue))))
   `(font-lock-keyword-face ((t (:foreground ,color-reddish-purple-dark))))
   `(font-lock-string-face ((t (:foreground ,color-bluish-green))))
   `(font-lock-type-face ((t (:foreground ,color-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,color-vermillion))))
   `(ansi-color-black ((t (:background "black" :foreground "black"))))
   `(ansi-color-red ((t (:background ,color-vermillion-dark :foreground ,color-vermillion-dark))))
   `(ansi-color-green ((t (:background ,color-bluish-green-dark :foreground ,color-bluish-green-dark))))
   `(ansi-color-yellow ((t (:background ,color-yellow-dark :foreground ,color-yellow-dark))))
   `(ansi-color-blue ((t (:background ,color-blue-dark :foreground ,color-blue-dark))))
   `(ansi-color-magenta ((t (:background ,color-reddish-purple-dark :foreground ,color-reddish-purple-dark))))
   `(ansi-color-cyan ((t (:background ,color-sky-blue-dark :foreground ,color-sky-blue-dark))))
   `(ansi-color-white ((t (:background "gray90" :foreground "gray90"))))
   `(ansi-color-bright-black ((t (:background "gray30" :foreground "gray30"))))
   `(ansi-color-bright-red ((t (:background ,color-vermillion :foreground ,color-vermillion))))
   `(ansi-color-bright-green ((t (:background ,color-bluish-green :foreground ,color-bluish-green))))
   `(ansi-color-bright-yellow ((t (:background ,color-yellow :foreground ,color-yellow))))
   `(ansi-color-bright-blue ((t (:background ,color-blue :foreground ,color-blue))))
   `(ansi-color-bright-magenta ((t (:background ,color-reddish-purple :foreground ,color-reddish-purple))))
   `(ansi-color-bright-cyan ((t (:background ,color-sky-blue :foreground ,color-sky-blue))))
   `(ansi-color-bright-white ((t (:background "white" :foreground "white"))))))

(provide-theme 'personal)
;;; personal-theme.el ends here
