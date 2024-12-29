;;; personal-theme.el --- Personal color theme -*- lexical-binding: t -*-

;;; Commentary:

;; Personal theme colors.

;;; Code:

(require 'color)

(deftheme personal "Personal theme." :background-mode 'light)

(defun personal-theme--color-255-to-1 (n)
  "Convert color component N ranging from number 0 to 255 to a number 0 to 1.0."
  (/ (fceiling (* 1000 (/ n 255.0))) 1000))

(defun personal-theme--color-65535-to-1 (n)
  "Convert color component N ranging from number 0 to 65535 to a number 0 to 1.0."
  (/ (fceiling (* 1000 (/ n 65535.0))) 1000))

(defun personal-theme--compute-int-color (r g b &optional adjust)
  "Get the hex string (#rrggbb) of the given R G B integers.
The given integers are expected to be between 0-255. Also it is possible
to lighten or darken the resulting color by ADJUST percent."
  (let* ((hsl (color-rgb-to-hsl
               (personal-theme--color-255-to-1 r)
               (personal-theme--color-255-to-1 g)
               (personal-theme--color-255-to-1 b)))
         (hsl-adjusted (color-lighten-hsl
                        (nth 0 hsl)
                        (nth 1 hsl)
                        (nth 2 hsl)
                        (if adjust adjust 0))))
    (apply #'color-rgb-to-hex
           (append (apply #'color-hsl-to-rgb hsl-adjusted) (list 2)))))

(defun personal-theme--compute-hex-color (base-hex &optional adjust)
  "Adjust BASE-HEX string (#rrggbb) by making it ADJUST percent lighter or darker."
  (let* ((rgb (color-values-from-color-spec base-hex))
         (hsl (color-rgb-to-hsl
               (personal-theme--color-65535-to-1 (nth 0 rgb))
               (personal-theme--color-65535-to-1 (nth 1 rgb))
               (personal-theme--color-65535-to-1 (nth 2 rgb))))
         (hsl-adjusted (color-lighten-hsl
                        (nth 0 hsl)
                        (nth 1 hsl)
                        (nth 2 hsl)
                        (if adjust adjust 0))))
    (apply #'color-rgb-to-hex
           (append (apply #'color-hsl-to-rgb hsl-adjusted) (list 2)))))

(defmacro personal-theme--colors-let (colorlist &rest body)
  "Bind colors from variables in COLORLIST then evaluate BODY.
Similar to a LET form, COLORLIST is expected to be a list of color
pairs, where each element is of the form (COLORNAME HEXVALUE).

Variants of a color pair are defined while executing BODY.  These
variants are named COLORNAME-1 up to COLORNAME-4, where 1 will be the
same as the base color while 4 will be the darkest variant.

There also is a light variant defined COLORNAME-light, which has 4
numbered variants the same as the base color."
  (declare (indent 1))
  `(let (,@(mapcan
            (lambda (colorpair)
              (let* ((name (car colorpair))
                     (hex-base (cadr colorpair))
                     (color-base (intern name))
                     (color-variant-1 (intern (format "%s-1" name)))
                     (color-variant-2 (intern (format "%s-2" name)))
                     (color-variant-3 (intern (format "%s-3" name)))
                     (color-variant-4 (intern (format "%s-4" name)))
                     (color-light-base (intern (format "%s-light" name)))
                     (color-light-variant-1 (intern (format "%s-light-1" name)))
                     (color-light-variant-2 (intern (format "%s-light-2" name)))
                     (color-light-variant-3 (intern (format "%s-light-3" name)))
                     (color-light-variant-4 (intern (format "%s-light-4" name))))
                `(;; base variants
                  (,color-base ,hex-base)
                  (,color-variant-1 ,(personal-theme--compute-hex-color hex-base 0)) ; base
                  (,color-variant-2 ,(personal-theme--compute-hex-color hex-base -10))
                  (,color-variant-3 ,(personal-theme--compute-hex-color hex-base -30))
                  (,color-variant-4 ,(personal-theme--compute-hex-color hex-base -50))
                  ;; light variants
                  (,color-light-base ,(personal-theme--compute-hex-color hex-base 40))
                  (,color-light-variant-1 ,(personal-theme--compute-hex-color hex-base 40)) ; light-base
                  (,color-light-variant-2 ,(personal-theme--compute-hex-color hex-base 30))
                  (,color-light-variant-3 ,(personal-theme--compute-hex-color hex-base 20))
                  (,color-light-variant-4 ,(personal-theme--compute-hex-color hex-base 10)))))
            colorlist))
     ,@body))

(personal-theme--colors-let
    (;; Use colors the same as `dichromacy'
     ("color-orange" "#e69f00")         ; orange
     ("color-cyan" "#56b4e9")           ; sky blue
     ("color-green" "#009e73")          ; bluish green
     ("color-yellow" "#f0e442")         ; yellow
     ("color-blue" "#0072b2")           ; blue
     ("color-red" "#d55e00")            ; vermillion
     ("color-magenta" "#cc79a7"))       ; reddish purple
  (let* (;; frame elements
         (bg-active color-magenta-light-2)
         (bg-active-border color-magenta-2)
         (bg-inactive "gray95")
         (bg-inactive-border "gray90")
         (bg-active-tab "white")
         (bg-inactive-tab "gray90")
         (fg-inactive "gray50")
         (box-active `(:line-width 2 :color ,bg-active-border :style flat-button))
         (box-inactive `(:line-width 2 :color ,bg-inactive-border :style flat-button))
         (box-active-tab `(:line-width 1 :color ,bg-active-tab :style released-button))
         (box-inactive-tab `(:line-width 1 :color ,bg-inactive-tab :style flat-button)))
    (custom-theme-set-faces
     'personal

     ;; faces
     `(cursor ((t (:background "black"))))
     `(default ((t (:font "Noto Sans Mono-10" :foreground "black" :background "white"))))
     `(fringe ((t (:background "gray98"))))
     `(highlight ((t (:background "gray95"))))
     `(minibuffer-prompt ((t (:foreground ,color-blue-2 :weight bold))))
     `(mode-line ((t (:background ,bg-active :box ,box-active))))
     `(mode-line-buffer-id ((t (:weight bold))))
     `(mode-line-inactive ((t (:background ,bg-inactive :box ,box-inactive :foreground ,fg-inactive))))
     `(region ((t (:background ,color-yellow))))
     `(secondary-selection ((t (:background ,color-yellow-light))))
     `(show-paren-match ((t (:background ,color-cyan-light-3))))
     `(show-paren-mismatch ((t (:background ,color-magenta-light-3))))
     `(variable-pitch ((t (:family "Noto Serif" :height 1.1))))
     `(window-divider ((t (:distant-foreground "gray80" :foreground "white"))))
     `(window-divider-first-pixel ((t (:distant-foreground "white" :foreground "white"))))
     `(window-divider-last-pixel ((t (:distant-foreground "white" :foreground "white"))))
     `(error ((t (:weight bold :foreground ,color-red))))
     `(warning ((t (:weight bold :foreground ,color-orange))))
     `(success ((t (:weight bold :foreground ,color-green))))
     `(link ((t (:foreground ,color-blue :underline t))))
     `(link-visited ((t (:inherit link :foreground ,color-magenta))))

     ;; adoc-mode
     `(adoc-meta-face ((t (:inherit default))))
     `(adoc-meta-hide-face ((t (:inherit adoc-meta-face :foreground "gray30"))))
     `(adoc-title-face ((t (:foreground ,color-blue :height 1.0 :weight bold))))
     `(adoc-title-0-face ((t (:inherit adoc-title-face :foreground ,color-blue :height 1.1))))
     `(adoc-title-1-face ((t (:inherit adoc-title-face :foreground ,color-green :height 1.1))))
     `(adoc-title-2-face ((t (:inherit adoc-title-face :foreground ,color-red :height 1.0))))
     `(adoc-title-3-face ((t (:inherit adoc-title-face :foreground ,color-magenta :height 1.0))))
     `(adoc-title-4-face ((t (:inherit adoc-title-face :foreground ,color-orange :height 1.0))))

     ;; ansi-color
     `(ansi-color-black ((t (:background "black" :foreground "black"))))
     `(ansi-color-red ((t (:background ,color-red :foreground ,color-red))))
     `(ansi-color-green ((t (:background ,color-green :foreground ,color-green))))
     `(ansi-color-yellow ((t (:background ,color-yellow :foreground ,color-yellow))))
     `(ansi-color-blue ((t (:background ,color-blue :foreground ,color-blue))))
     `(ansi-color-magenta ((t (:background ,color-magenta :foreground ,color-magenta))))
     `(ansi-color-cyan ((t (:background ,color-cyan :foreground ,color-cyan))))
     `(ansi-color-white ((t (:background "gray90" :foreground "gray90"))))
     `(ansi-color-bright-black ((t (:background "gray30" :foreground "gray30"))))
     `(ansi-color-bright-red ((t (:background ,color-red-light :foreground ,color-red-light))))
     `(ansi-color-bright-green ((t (:background ,color-green-light :foreground ,color-green-light))))
     `(ansi-color-bright-yellow ((t (:background ,color-yellow-light :foreground ,color-yellow-light))))
     `(ansi-color-bright-blue ((t (:background ,color-blue-light :foreground ,color-blue-light))))
     `(ansi-color-bright-magenta ((t (:background ,color-magenta-light :foreground ,color-magenta-light))))
     `(ansi-color-bright-cyan ((t (:background ,color-cyan-light :foreground ,color-cyan-light))))
     `(ansi-color-bright-white ((t (:background "white" :foreground "white"))))

     ;; avy
     `(avy-lead-face ((t (:background ,color-orange-light :foreground "black"))))
     `(avy-lead-face-0 ((t (:background ,color-orange-light :foreground "black"))))
     `(avy-lead-face-1 ((t (:background ,color-orange-light :foreground "black"))))
     `(avy-lead-face-2 ((t (:background ,color-orange-light :foreground "black"))))

     ;; completions, icomplete
     `(completions-common-part ((t (:foreground ,color-blue-3))))
     `(completions-highlight ((t (:background ,color-yellow-light))))
     `(icomplete-selected-match ((t (:background ,color-yellow-light))))

     ;; diff-hl, diff-mode
     `(diff-hl-change ((t (:background "aliceblue" :foreground ,color-blue))))
     `(diff-added ((t (:inherit diff-changed :extend t :background "honeydew" :foreground ,color-green))))
     `(diff-removed ((t (:inherit diff-changed :extend t :background "mistyrose" :foreground ,color-red))))

     ;; emms
     `(emms-metaplaylist-mode-face ((t (:inherit default :weight normal))))
     `(emms-metaplaylist-mode-current-face ((t (:background ,color-cyan-light :weight bold))))
     `(emms-playlist-track-face ((t (:inherit default))))
     `(emms-playlist-selected-face ((t (:inherit emms-playlist-track-face :background ,color-cyan-light :weight bold))))

     ;; eshell
     `(eshell-ls-archive ((t (:foreground ,color-magenta :weight bold))))
     `(eshell-ls-backup ((t (:foreground ,color-red-light-4))))
     `(eshell-ls-clutter ((t (:foreground ,color-red-light-4 :weight bold))))
     `(eshell-ls-executable ((t (:foreground ,color-green :weight bold))))
     `(eshell-ls-missing ((t (:foreground ,color-red :weight bold))))
     `(eshell-ls-product ((t (:foreground ,color-red-light-4))))
     `(eshell-ls-readonly ((t (:foreground ,color-orange-3))))
     `(eshell-ls-special ((t (:foreground ,color-magenta :weight bold))))
     `(eshell-prompt ((t (:foreground ,color-red :weight bold))))

     ;; font-lock
     `(font-lock-builtin-face ((t (:foreground ,color-blue-3))))
     `(font-lock-comment-face ((t (:foreground ,color-orange-3))))
     `(font-lock-constant-face ((t (:foreground ,color-cyan-3))))
     `(font-lock-function-name-face ((t (:foreground ,color-blue-3))))
     `(font-lock-keyword-face ((t (:foreground ,color-magenta-3))))
     `(font-lock-string-face ((t (:foreground ,color-green-3))))
     `(font-lock-type-face ((t (:foreground ,color-blue-3))))
     `(font-lock-variable-name-face ((t (:foreground ,color-red-3))))

     ;; gnus
     `(gnus-group-mail-1-empty ((t (:foreground ,color-red-3))))
     `(gnus-group-mail-2-empty ((t (:foreground ,color-red))))
     `(gnus-group-mail-3-empty ((t (:foreground ,color-magenta-3))))
     `(gnus-group-mail-low-empty ((t (:foreground ,color-magenta))))
     `(gnus-group-news-1-empty ((t (:foreground ,color-green-3))))
     `(gnus-group-news-2-empty ((t (:foreground ,color-green))))
     `(gnus-group-news-3-empty ((t (:foreground ,color-blue-3))))
     `(gnus-group-news-low-empty ((t (:foreground ,color-blue))))
     `(gnus-header ((t (:family "Noto Sans" :height 1.1))))
     `(gnus-header-content ((t (:inherit gnus-header :foreground ,color-green-2))))
     `(gnus-header-from ((t (:inherit gnus-header :foreground ,color-red-2))))
     `(gnus-header-name ((t (:inherit gnus-header :foreground ,color-magenta-2))))
     `(gnus-header-newsgroups ((t (:foreground ,color-blue-4))))
     `(gnus-header-subject ((t (:inherit gnus-header :foreground ,color-red-3))))

     ;; howm
     `(action-lock-face ((t (:underline t))))
     `(howm-menu-key-face ((t (:foreground ,color-red-3))))
     `(howm-mode-keyword-face ((t (:foreground ,color-blue))))
     `(howm-mode-ref-face ((t (:foreground ,color-blue))))
     `(howm-mode-title-face ((t (:weight bold))))
     `(howm-mode-wiki-face ((t (:foreground ,color-blue))))
     `(howm-reminder-deadline-face ((t (:foreground ,color-red-2))))
     `(howm-reminder-defer-face ((t (:foreground ,color-magenta-2))))
     `(howm-reminder-normal-face ((t (:foreground ,color-blue-2))))
     `(howm-reminder-schedule-face ((t (:foreground ,color-green-2))))
     `(howm-reminder-todo-face ((t (:foreground ,color-orange-2))))
     `(howm-reminder-late-deadline-face ((t (:inverse-video t :weight bold))))
     `(howm-reminder-today-face ((t (:background "lightyellow1" :box (:color "gray" :line-width (-1 . -1))))))
     `(howm-reminder-tomorrow-face ((t (:box (:color "gray" :line-width (-1 . -1))))))
     `(howm-view-empty-face ((t (:inherit default))))
     `(howm-view-hilit-face ((t (:foreground ,color-red))))
     `(howm-view-name-face ((t (:inherit default))))

     ;; isearch
     `(isearch ((t (:foreground "white" :background ,color-magenta-2))))
     `(isearch-fail ((t (:background ,color-red-light))))
     `(lazy-highlight ((t (:distant-foreground "black" :background ,color-cyan-light))))

     ;; org, org-modern, org-roam
     `(org-block ((t (:inherit shadow :extend t :background "gray97" :foreground "gray40"))))
     `(org-block-begin-line ((t (:inherit org-meta-line :extend t :background "gray97"))))
     `(org-block-end-line ((t (:inherit org-meta-line :extend t :background "gray97"))))
     `(org-date ((t (:inherit default :background ,color-magenta-light :height 0.9 :underline t))))
     `(org-done ((t (:weight normal))))
     `(org-drawer ((t (:height 0.85 :foreground "gray50"))))
     `(org-level-1 ((t (:inherit outline-1 :height 1.1 :foreground ,color-blue))))
     `(org-level-2 ((t (:inherit outline-2 :height 1.1 :foreground ,color-green))))
     `(org-level-3 ((t (:inherit outline-3 :height 1.0 :foreground ,color-red))))
     `(org-level-4 ((t (:inherit outline-4 :height 1.0 :foreground ,color-magenta))))
     `(org-level-5 ((t (:inherit outline-5 :height 1.0 :foreground ,color-orange))))
     `(org-level-6 ((t (:inherit outline-6 :height 1.0 :foreground ,color-cyan))))
     `(org-level-7 ((t (:inherit outline-7 :height 1.0 :foreground ,color-yellow-3))))
     `(org-modern-label ((t (:height 0.85))))
     `(org-property-value ((t (:height 0.85 :foreground "gray50"))))
     `(org-roam-dailies-calendar-note ((t (:inherit org-link :weight bold))))
     `(org-roam-header-line ((t (:background ,color-yellow-light :foreground ,color-yellow-4 :extend t :weight bold))))
     `(org-roam-title ((t (:weight bold :background ,color-cyan-light))))
     `(org-special-keyword ((t (:height 0.85 :foreground "gray50"))))
     `(org-table ((t (:foreground ,color-blue-2 :background "gray97"))))
     `(org-todo ((t (:weight normal))))

     ;; orderless
     `(orderless-match-face-0 ((t (:foreground ,color-red-2 :weight bold))))
     `(orderless-match-face-1 ((t (:foreground ,color-magenta-2 :weight bold))))
     `(orderless-match-face-2 ((t (:foreground ,color-green-2 :weight bold))))
     `(orderless-match-face-3 ((t (:foreground ,color-orange-2 :weight bold))))

     ;; tab-bar
     `(tab-bar ((t (:inherit default :background "gray97"))))
     `(tab-bar-tab ((t (:inherit tab-bar :weight bold :background ,bg-active-tab :box ,box-active-tab))))
     `(tab-bar-tab-inactive ((t (:inherit tab-bar :background ,bg-inactive-tab :box ,box-inactive-tab :foreground ,fg-inactive))))

     ;; yas
     `(yas-field-highlight-face ((t (:inherit minibuffer-prompt)))))))

(provide-theme 'personal)
;;; personal-theme.el ends here
