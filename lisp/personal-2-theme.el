;;; personal-2-theme.el --- Personal color theme 2 -*- lexical-binding: t -*-

;;; Commentary:

;; Personal theme colors, based on Paul Tol's color schemes.

;;; Code:

(require 'color)

(deftheme personal-2 "Personal theme 2." :background-mode 'light)

(defun personal-theme--color-255-to-1 (n)
  "Convert color component N ranging from number 0 to 255 to a number 0 to 1.0."
  (/ (fceiling (* 1000 (/ n 255.0))) 1000))

(defun personal-theme--color-65535-to-1 (n)
  "Convert color component N ranging from number 0 to 65535 to a number 0 to 1.0."
  (/ (fceiling (* 1000 (/ n 65535.0))) 1000))

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
                     (color-variant-4 (intern (format "%s-4" name))))
                `(;; base variants
                  (,color-base ,hex-base)
                  (,color-variant-1 ,(personal-theme--compute-hex-color hex-base 0)) ; base
                  (,color-variant-2 ,(personal-theme--compute-hex-color hex-base -10))
                  (,color-variant-3 ,(personal-theme--compute-hex-color hex-base -30))
                  (,color-variant-4 ,(personal-theme--compute-hex-color hex-base -50)))))
            colorlist))
     ,@body))

(personal-theme--colors-let
    (("color-white" "#ffffff")
     ("color-black" "#000000")
     ;; bright (foregrounds)
     ("color-blue" "#4477aa")
     ("color-red" "#ee6677")
     ("color-green" "#228833")
     ("color-yellow" "#ccbb44")
     ("color-cyan" "#66ccee")
     ("color-purple" "#aa3377")
     ("color-gray" "#bbbbbb")
     ;; light (foregrounds)
     ("color-light-blue" "#77aadd")
     ("color-light-cyan" "#99ddff")
     ("color-mint" "#44bb99")
     ("color-pear" "#bbcc33")
     ("color-olive" "#aaaa00")
     ("color-light-yellow" "#eedd88")
     ("color-orange" "#ee8866")
     ("color-pink" "#ffaabb")
     ;; pale (backgrounds)
     ("color-pale-blue" "#bbccee")
     ("color-pale-cyan" "#cceeff")
     ("color-pale-green" "#ccddaa")
     ("color-pale-yellow" "#eeeebb")
     ("color-pale-red" "#ffcccc")
     ("color-pale-gray" "#dddddd"))
  (let* ((fg-inactive color-gray-4)
         (font "Inconsolata-11")
         (flat-box `(:line-width 3 :style flat-button)))
    (custom-theme-set-faces
     'personal-2

     ;; faces
     `(cursor ((t (:background ,color-red-2))))
     `(default ((t (:font ,font :foreground ,color-black :background ,color-white))))
     `(display-time-date-and-time ((t (:foreground ,color-blue))))
     `(fixed-pitch ((t (:font ,font))))
     `(fringe ((t (:background "gray95"))))
     `(highlight ((t (:background ,color-pale-yellow))))
     `(hl-line ((t (:underline ,color-gray))))
     `(minibuffer-prompt ((t (:foreground ,color-blue :weight bold))))
     `(mode-line ((t (:background ,color-pale-blue :box ,flat-box))))
     `(mode-line-buffer-id ((t (:weight bold))))
     `(mode-line-inactive ((t (:background ,color-pale-gray :box ,flat-box :foreground ,fg-inactive))))
     `(region ((t (:background ,color-pale-yellow))))
     `(secondary-selection ((t (:background ,color-pale-gray))))
     `(show-paren-match ((t (:background ,color-pale-green))))
     `(show-paren-mismatch ((t (:background ,color-red-2 :foreground ,color-white))))
     `(variable-pitch ((t (:family "Noto Serif" :height 1.1))))
     `(window-divider ((t (:foreground ,color-black))))
     `(window-divider-first-pixel ((t (:distant-foreground "white" :foreground "white"))))
     `(window-divider-last-pixel ((t (:distant-foreground "white" :foreground "white"))))
     `(error ((t (:weight bold :foreground ,color-red))))
     `(warning ((t (:weight bold :foreground ,color-orange))))
     `(success ((t (:weight bold :foreground ,color-green))))
     `(link ((t (:foreground ,color-blue :underline t))))
     `(link-visited ((t (:inherit link :foreground ,color-purple))))

     ;; adoc-mode
     `(adoc-meta-face ((t (:inherit default))))
     `(adoc-meta-hide-face ((t (:inherit adoc-meta-face :foreground ,color-gray))))
     `(adoc-title-face ((t (:foreground ,color-blue :height 1.0 :weight bold))))
     `(adoc-title-0-face ((t (:inherit adoc-title-face :foreground ,color-blue :height 1.1))))
     `(adoc-title-1-face ((t (:inherit adoc-title-face :foreground ,color-green :height 1.1))))
     `(adoc-title-2-face ((t (:inherit adoc-title-face :foreground ,color-red :height 1.0))))
     `(adoc-title-3-face ((t (:inherit adoc-title-face :foreground ,color-purple :height 1.0))))
     `(adoc-title-4-face ((t (:inherit adoc-title-face :foreground ,color-orange :height 1.0))))

     ;; avy
     `(avy-lead-face ((t (:background ,color-pale-red :foreground ,color-black :weight bold))))
     `(avy-lead-face-0 ((t (:background ,color-pale-red :foreground ,color-black))))
     `(avy-lead-face-1 ((t (:background ,color-pale-red :foreground ,color-black))))
     `(avy-lead-face-2 ((t (:background ,color-pale-red :foreground ,color-black))))

     ;; completions, icomplete
     `(completions-common-part ((t (:foreground ,color-blue))))
     `(completions-highlight ((t (:background ,color-pale-yellow))))
     `(icomplete-selected-match ((t (:foreground ,color-black :background ,color-pale-yellow :weight bold))))

     ;; diff-hl, diff-mode
     `(diff-hl-change ((t (:background ,color-pale-blue :foreground ,color-blue))))
     `(diff-added ((t (:inherit diff-changed :extend t :background ,color-pale-green :foreground ,color-green))))
     `(diff-removed ((t (:inherit diff-changed :extend t :background ,color-pale-red :foreground ,color-red))))

     ;; emms
     `(emms-metaplaylist-mode-face ((t (:inherit default :weight normal))))
     `(emms-metaplaylist-mode-current-face ((t (:foreground ,color-red :weight bold))))
     `(emms-playlist-track-face ((t (:inherit default))))
     `(emms-playlist-selected-face ((t (:inherit emms-playlist-track-face :foreground ,color-red :weight bold))))

     ;; eshell
     `(eshell-ls-archive ((t (:foreground ,color-purple :weight bold))))
     `(eshell-ls-backup ((t (:foreground ,color-orange))))
     `(eshell-ls-clutter ((t (:foreground ,color-orange :weight bold))))
     `(eshell-ls-executable ((t (:foreground ,color-green :weight bold))))
     `(eshell-ls-missing ((t (:foreground ,color-red :weight bold))))
     `(eshell-ls-product ((t (:foreground ,color-orange))))
     `(eshell-ls-readonly ((t (:foreground ,color-pink))))
     `(eshell-ls-special ((t (:foreground ,color-purple :weight bold))))
     `(eshell-prompt ((t (:foreground ,color-red :weight bold))))

     ;; font-lock
     `(font-lock-builtin-face ((t (:foreground ,color-blue))))
     `(font-lock-comment-face ((t (:foreground ,color-red-3))))
     `(font-lock-constant-face ((t (:foreground ,color-olive))))
     `(font-lock-function-name-face ((t (:foreground ,color-blue))))
     `(font-lock-keyword-face ((t (:foreground ,color-purple))))
     `(font-lock-string-face ((t (:foreground ,color-green))))
     `(font-lock-type-face ((t (:foreground ,color-mint))))
     `(font-lock-variable-name-face ((t (:foreground ,color-red))))

     ;; gnus
     `(gnus-group-mail-1-empty ((t (:foreground ,color-red))))
     `(gnus-group-mail-2-empty ((t (:foreground ,color-red))))
     `(gnus-group-mail-3-empty ((t (:foreground ,color-purple))))
     `(gnus-group-mail-low-empty ((t (:foreground ,color-purple))))
     `(gnus-group-news-1-empty ((t (:foreground ,color-green))))
     `(gnus-group-news-2-empty ((t (:foreground ,color-green))))
     `(gnus-group-news-3-empty ((t (:foreground ,color-blue))))
     `(gnus-group-news-low-empty ((t (:foreground ,color-blue))))
     `(gnus-header ((t (:family "Noto Sans" :height 1.1))))
     `(gnus-header-content ((t (:inherit gnus-header :foreground ,color-green))))
     `(gnus-header-from ((t (:inherit gnus-header :foreground ,color-red))))
     `(gnus-header-name ((t (:inherit gnus-header :foreground ,color-purple))))
     `(gnus-header-newsgroups ((t (:foreground ,color-blue))))
     `(gnus-header-subject ((t (:inherit gnus-header :foreground ,color-red))))

     ;; howm
     `(action-lock-face ((t (:underline t))))
     `(howm-menu-key-face ((t (:foreground ,color-black :background ,color-pale-yellow :weight bold))))
     `(howm-mode-keyword-face ((t (:foreground ,color-black :background ,color-pale-yellow :weight bold))))
     `(howm-mode-ref-face ((t (:foreground ,color-blue))))
     `(howm-mode-title-face ((t (:inherit outline-1))))
     `(howm-mode-wiki-face ((t (:foreground ,color-blue))))
     `(howm-reminder-deadline-face ((t (:foreground ,color-red))))
     `(howm-reminder-defer-face ((t (:foreground ,color-purple))))
     `(howm-reminder-normal-face ((t (:foreground ,color-blue))))
     `(howm-reminder-schedule-face ((t (:foreground ,color-green))))
     `(howm-reminder-todo-face ((t (:foreground ,color-orange))))
     `(howm-reminder-late-deadline-face ((t (:weight bold :foreground ,color-red-2 :background ,color-pale-red))))
     `(howm-reminder-today-face ((t (:background ,color-pale-green))))
     `(howm-reminder-tomorrow-face ((t (:background ,color-pale-gray))))
     `(howm-view-empty-face ((t (:inherit default :background ,color-white))))
     `(howm-view-hilit-face ((t (:background ,color-pale-cyan))))
     `(howm-view-name-face ((t (:inherit default :background ,color-white))))

     ;; isearch
     `(isearch ((t (:background ,color-pale-yellow))))
     `(isearch-fail ((t (:background ,color-pale-red))))
     `(lazy-highlight ((t (:distant-foreground ,color-black :background ,color-pale-gray))))

     ;; org, org-modern, org-roam
     `(org-block ((t (:inherit (fixed-pitch shadow) :extend t :background "gray95" :foreground ,color-black))))
     `(org-block-begin-line ((t (:inherit (fixed-pitch) :foreground ,color-blue :extend t :background "gray95" :overline t))))
     `(org-block-end-line ((t (:inherit (fixed-pitch) :foreground ,color-blue :extend t :background "gray95" :underline t))))
     `(org-checkbox ((t (:inherit default))))
     `(org-code ((t (:inherit (fixed-pitch shadow)))))
     `(org-date ((t (:height 1.0 :underline nil))))
     `(org-done ((t (:weight normal))))
     `(org-drawer ((t (:height 0.85 :foreground ,color-gray))))
     `(org-level-1 ((t (:inherit outline-1 :height 1.1 :weight bold :overline "gray50"))))
     `(org-level-2 ((t (:inherit outline-2 :height 1.0 :weight bold :overline "gray70"))))
     `(org-level-3 ((t (:inherit outline-3 :height 1.0 :weight bold))))
     `(org-level-4 ((t (:inherit outline-4 :height 1.0))))
     `(org-level-5 ((t (:inherit outline-5 :height 1.0))))
     `(org-level-6 ((t (:inherit outline-6 :height 1.0))))
     `(org-level-7 ((t (:inherit outline-7 :height 1.0))))
     `(org-modern-label ((t (:height 0.85))))
     `(org-property-value ((t (:height 0.85 :foreground ,color-black))))
     `(org-roam-dailies-calendar-note ((t (:inherit org-link :weight bold))))
     `(org-roam-header-line ((t (:background ,color-pale-yellow :foreground ,color-yellow :extend t :weight bold))))
     `(org-roam-title ((t (:weight bold :background ,color-pale-cyan))))
     `(org-special-keyword ((t (:height 0.85 :foreground ,color-black))))
     `(org-table ((t (:foreground ,color-black :background ,color-white))))
     `(org-tag ((t (:height 1.0 :foreground ,color-black))))
     `(org-todo ((t (:weight normal))))
     `(org-verbatim ((t (:inherit (fixed-pitch shadow)))))

     ;; orderless
     `(orderless-match-face-0 ((t (:foreground ,color-red :weight bold))))
     `(orderless-match-face-1 ((t (:foreground ,color-purple :weight bold))))
     `(orderless-match-face-2 ((t (:foreground ,color-green :weight bold))))
     `(orderless-match-face-3 ((t (:foreground ,color-orange :weight bold))))

     ;; tab-bar
     `(tab-bar ((t (:inherit default :background ,color-pale-gray))))
     `(tab-bar-tab ((t (:inherit tab-bar :weight bold :background ,color-white :box ,flat-box))))
     `(tab-bar-tab-inactive ((t (:inherit tab-bar :background ,color-pale-gray :box ,flat-box :foreground ,fg-inactive))))

     ;; yas
     `(yas-field-highlight-face ((t (:inherit minibuffer-prompt)))))))

(provide-theme 'personal-2)
;;; personal-2-theme.el ends here
