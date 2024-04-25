;;; init.el --- Nicolai's Emacs init file

;;; Commentary:

;; This contains my personal Emacs settings.

;;; Code:

;;;; GC setup

(setq original-gc-cons-percentage gc-cons-percentage
      higher-gc-cons-percentage 0.75)
(setq gc-idle-timer nil)

(defun start-gc-idle-timer ()
  "Set a timer that GCs when Emacs idles, like package gcmh."
  (when (timerp gc-idle-timer) (cancel-timer gc-idle-timer))
  (setq gc-idle-timer (run-with-idle-timer 5 t #'garbage-collect)))

(defun increase-gc-cons-percentage ()
  "Set a higher value for gc-cons-percentage to prevent garbage
collection.  Use revert-gc-cons-percentage to restore the value."
  (setq gc-cons-percentage higher-gc-cons-percentage))

(defun revert-gc-cons-percentage ()
  "Restore gc-cons-percentage to its original value."
  (setq gc-cons-percentage original-gc-cons-percentage))

(start-gc-idle-timer)
(increase-gc-cons-percentage)
(add-hook 'after-init-hook
          (lambda ()
            (revert-gc-cons-percentage)
            (garbage-collect)
            (setq garbage-collection-messages nil)))

(add-hook 'minibuffer-setup-hook #'increase-gc-cons-percentage)
(add-hook 'minibuffer-exit-hook #'revert-gc-cons-percentage)
(add-hook 'isearch-mode-hook #'increase-gc-cons-percentage)
(add-hook 'isearch-mode-end-hook #'revert-gc-cons-percentage)

;;;; Emacs-wide initializations

(add-hook 'emacs-startup-hook
          (lambda ()
            (message (concat "Emacs startup took " (emacs-init-time) " with " (number-to-string gcs-done) " GCs."))))

;; (add-hook 'after-init-hook #'server-start)

;; General emacs settings
(setq
 user-full-name "Nicolai Singh"
 user-mail-address "nicolaisingh@pm.me"

 ;; Timeout in seconds before auto-save
 auto-save-timeout 5

 ;; How much input before auto-saving
 auto-save-interval 100

 ;; Seconds before paren matches are highlighted
 show-paren-delay 1

 ;; Show N lines of text on top/bottom when scrolling
 scroll-margin 0

 ;; Allow C-SPC to continue popping the mark after C-u C-SPC
 set-mark-command-repeat-pop t

 ;; Ask when exiting emacs
 confirm-kill-emacs #'yes-or-no-p

 ;; Do not ask to follow symlinks; always follow
 vc-follow-symlinks t

 ;; Save bookmarks immediately
 bookmark-save-flag t

 ;; No duplicates in minibuffer history
 history-delete-duplicates t

 ;; Horizontal scroll the current line only
 auto-hscroll-mode 'current-line

 ;; Don't save if it duplicates the last one in the kill ring
 kill-do-not-save-duplicates t

 ;; Kill whole lines
 kill-whole-line t

 ;; C-n adds new lines at eof
 next-line-add-newlines t

 ;; Save clipboard contents before killing
 save-interprogram-paste-before-kill t

 ;; Don't ask when reverting for the following regex
 revert-without-query '("^.*\\.pdf$")

 abbrev-file-name (expand-file-name "abbrev-defs.el" user-emacs-directory)

 help-window-select t)

;; Display long lines as is
(setq-default truncate-lines t)

;; Always enable the following functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(repeat-mode 1)
(winner-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers 1)
(minibuffer-depth-indicate-mode 1)

;; Where to store backup and autosave files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups/")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosaves/") t)))

;; Buffer placement
(setq same-window-regexps
      '("^magit: .*$"
        "^magit-status: .*$"))

;; Tabs and spaces handling
(setq-default tab-width 4)
(add-hook 'shell-mode-hook #'indent-tab-width-8)
(add-hook 'emacs-lisp-mode-hook #'indent-tab-width-8)
(add-hook 'kotlin-mode-hook #'indent-spaces)
(add-hook 'emacs-lisp-mode-hook #'indent-spaces)
(add-hook 'scheme-mode-hook #'indent-spaces)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Don't wrap minibuffer entries
(add-hook 'minibuffer-setup-hook (lambda () (setq-local truncate-lines t)))

;; Move facemenu-keymap to another binding
(require 'facemenu)
(keymap-global-unset "M-o")
(keymap-global-set "C-c f m" #'facemenu-keymap)

;; Prefix definitions
(define-prefix-command 'my-debug-map)
(define-prefix-command 'my-ctl-c-M-map)
(define-prefix-command 'my-ctl-c-b-map)
(define-prefix-command 'my-ctl-c-d-map)
(define-prefix-command 'my-ctl-c-e-map)
(define-prefix-command 'my-ctl-c-f-map)
(define-prefix-command 'my-ctl-c-h-map)
(define-prefix-command 'my-ctl-c-i-map)
(define-prefix-command 'my-ctl-c-l-map)
(define-prefix-command 'my-ctl-c-m-map)
(define-prefix-command 'my-ctl-c-n-map)
(define-prefix-command 'my-ctl-c-o-map)
(define-prefix-command 'my-ctl-c-p-map)
(define-prefix-command 'my-ctl-c-s-map)
(define-prefix-command 'my-ctl-c-t-map)
(define-prefix-command 'my-ctl-c-v-map)
(define-prefix-command 'my-ctl-c-w-map)
(define-prefix-command 'my-ctl-c-y-map)
(define-prefix-command 'my-ctl-z-map)

(keymap-global-set "C-c D" 'my-debug-map)
(keymap-global-set "C-c M" 'my-ctl-c-M-map)
(keymap-global-set "C-c b" 'my-ctl-c-b-map)
(keymap-global-set "C-c d" 'my-ctl-c-d-map)
(keymap-global-set "C-c e" 'my-ctl-c-e-map)
(keymap-global-set "C-c f" 'my-ctl-c-f-map)
(keymap-global-set "C-c h" 'my-ctl-c-h-map)
(keymap-global-set "C-c i" 'my-ctl-c-i-map)
(keymap-global-set "C-c l" 'my-ctl-c-l-map)
(keymap-global-set "C-c m" 'my-ctl-c-m-map)
(keymap-global-set "C-c n" 'my-ctl-c-n-map)
(keymap-global-set "C-c o" 'my-ctl-c-o-map)
(keymap-global-set "C-c p" 'my-ctl-c-p-map)
(keymap-global-set "C-c s" 'my-ctl-c-s-map)
(keymap-global-set "C-c t" 'my-ctl-c-t-map)
(keymap-global-set "C-c v" 'my-ctl-c-v-map)
(keymap-global-set "C-c w" 'my-ctl-c-w-map)
(keymap-global-set "C-c y" 'my-ctl-c-y-map)
(keymap-global-set "C-z" 'my-ctl-z-map)

;;;; Package initializations

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'mode-local)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)
(require 'init-theme)
(require 'init-etc)

(ensure-all-packages-installed)

(require 'init-gui)
(require 'init-treesit)

(require 'window-dedicated)
(keymap-global-set "C-c w d" #'window-dedicated-mode)

(require 'winfast)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-`" #'winfast-mode)

(require 'init-alert)
(require 'init-aggressive-indent)
(require 'init-browse-kill-ring)
(require 'init-calibre)
;; (require 'init-bs)
(require 'init-chatgpt-shell)
(require 'init-chronos)
(require 'init-command-log)
;; (require 'init-company)
(require 'init-consult)
(require 'init-corfu-cape)
(require 'init-cov)
(require 'init-devdocs)
(require 'init-diff-hl)
(require 'init-diminish)
(require 'init-dired)
(require 'init-dired-toggle)
(require 'init-ediff)
(require 'init-eglot)
(require 'init-emms)
(require 'init-erc)
(require 'init-eshell)
(require 'init-expand-region)
(require 'init-flycheck)
(require 'init-graphviz-dot-mode)
(require 'init-highlight-indent-guides)
(require 'init-hippie-expand)
(require 'init-ibuffer)
(require 'init-icomplete)
;; (require 'init-ido)
(require 'init-imenu-list)
(require 'init-isearch)
;; (require 'init-ivy)
(require 'init-json-navigator)
(require 'init-latex)
(require 'init-magit)
(require 'init-mail)
(require 'init-marginalia)
(require 'init-markdown)
(require 'init-multi-term)
(require 'init-multiple-cursors)
(require 'init-native-complete)
(require 'init-nav-flash)
(require 'init-newsticker)
(require 'init-nov)
(require 'init-orderless)
(require 'init-org)
(require 'init-org-alert)
(require 'init-org-crypt)
(require 'init-org-modern)
(require 'init-org-present)
(require 'init-origami)
(require 'init-pdf)
(require 'init-plantuml)
;; (require 'init-prism)
(require 'init-prodigy)
(require 'init-pytest)
(require 'init-recentf)
(require 'init-restclient)
(require 'init-selected)
(require 'init-slime)
(require 'init-smartparens)
(require 'init-transpose-frame)
;; (require 'init-which-key)
(require 'init-xref)
(require 'init-yasnippet)

;; Languages
(require 'init-clojurescript)
(require 'init-go)
(require 'init-javascript)
(require 'init-kotlin)
(require 'init-nix)
(require 'init-python)
(require 'init-scheme)
(require 'init-solidity)
(require 'init-typescript)

(with-eval-after-load 'init-org
  (require 'init-org-modern))
(with-eval-after-load 'init-org
  (require 'init-org-roam))

;;;; Other key bindings
(keymap-global-set "<remap> <kill-ring-save>" #'easy-kill)
(keymap-global-set "<remap> <mark-word>" #'easy-mark)

(keymap-global-set "C-x t T" #'tab-bar-mode)

(keymap-set my-ctl-z-map "C-s" #'eshell-toggle)
(keymap-set my-ctl-z-map "C-z" #'org-capture-inbox)
(keymap-set my-ctl-z-map "g" #'magit-status)
(keymap-set my-ctl-z-map "c" #'org-capture)
(keymap-set my-ctl-z-map "C-l" #'chatgpt-shell)

(keymap-set my-debug-map "q" #'toggle-debug-on-quit)
(keymap-set my-debug-map "e" #'toggle-debug-on-error)
(keymap-set my-debug-map "p p" #'profiler-toggle)
(keymap-set my-debug-map "p r" #'profiler-report)
(keymap-set my-debug-map "t" #'debug-on-entry)
(keymap-set my-debug-map "T" #'cancel-debug-on-entry)
(keymap-set my-debug-map "v" #'debug-on-variable-change)
(keymap-set my-debug-map "V" #'cancel-debug-on-variable-change)

(keymap-global-set "C-c d l" #'dictionary-search)
(keymap-global-set "C-c h l" #'hl-line-mode)
(keymap-global-set "C-c l d" #'duplicate-line)
(keymap-global-set "C-c v f" #'virtual-auto-fill-mode)
(keymap-global-set "C-c w '" #'insert-pair)
(keymap-global-set "C-c w <" #'insert-pair)
(keymap-global-set "C-c w [" #'insert-pair)
(keymap-global-set "C-c w \"" #'insert-pair)
(keymap-global-set "C-h C-k" #'describe-keymap)
(keymap-global-set "C-h u f" #'find-library)
(keymap-global-set "C-h u p" #'list-packages)
(keymap-global-set "C-x B" #'bury-buffer)
(keymap-global-set "C-x C-S-c" #'save-buffers-kill-emacs)
(keymap-global-set "C-x C-m" (key-binding (kbd "M-x")))
(keymap-global-set "C-x K" #'kill-this-buffer)
(keymap-global-set "C-x a /" #'unexpand-abbrev)
(keymap-global-set "M-SPC" #'cycle-spacing)
(keymap-global-unset "C-h C-c")
(keymap-global-unset "C-h C-f")

(provide 'init)
;;; init.el ends here
