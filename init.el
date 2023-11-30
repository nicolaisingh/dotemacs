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

;; (add-hook 'after-init-hook #'server-start)

;; General emacs settings
(setq
 user-full-name "Nicolai Singh"
 user-mail-address "nicolaisingh@pm.me"

 ;; Timeout in seconds before auto-save
 auto-save-timeout 5

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

 ;; Horizontal scroll the current line only
 auto-hscroll-mode 'current-line

 ;; Don't save if it duplicates the last one in the kill ring
 kill-do-not-save-duplicates t

 ;; Kill whole lines
 kill-whole-line t

 ;; C-n adds new lines at eof
 next-line-add-newlines t

 ;; Save clipboard contents before killing
 save-interprogram-paste-before-kill t)

;; Don't ask when reverting for the following regex
(setq revert-without-query '("^.*\\.pdf$"))

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

;; Tab bar
(global-set-key (kbd "C-x t T") #'tab-bar-mode)

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

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)

;; Other key bindings
(global-set-key (kbd "C-c h l") #'hl-line-mode)
(global-set-key (kbd "C-x C-S-c") #'save-buffers-kill-emacs)
(global-set-key (kbd "M-SPC") #'cycle-spacing)
(global-set-key (kbd "C-x C-m") (key-binding (kbd "M-x")))
(global-set-key (kbd "C-x B") #'bury-buffer)
(global-set-key (kbd "C-z") #'repeat)
(global-set-key (kbd "H-D") (lambda ()
                              (interactive)
                              (browse-url-xdg-open (xdg-user-dir "DOWNLOAD"))))
(global-set-key (kbd "H-d") (lambda ()
                              (interactive)
                              (dired "~/Downloads")))
(global-set-key (kbd "C-h u f") #'find-library)
(global-set-key (kbd "C-h u p") #'list-packages)
(global-set-key (kbd "C-c w '") #'insert-pair)
(global-set-key (kbd "C-c w \"") #'insert-pair)

;; Move facemenu-keymap to another binding
(require 'facemenu)
(define-key global-map (kbd "M-o") nil) ;; winfast and other-window will occupy M-o
(define-key global-map (kbd "C-c f m") 'facemenu-keymap)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message (concat "Emacs startup took " (emacs-init-time) " with " (number-to-string gcs-done) " GCs."))))

;;;; Load Customize file

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;; Macros
(fset 'nft-fix-jsons
      (kmacro-lambda-form [?w return ?\C-x ?h backspace ?\{
                              return ?\" ?n ?a ?m ?e ?: S-backspace ?\" ?: ?\S- ?\" ?\C-y
                              ?\C-b ?\C-b ?\C-b ?\C-b ?\C-b ?\C-d ?\C-d ?\C-d ?\C-d ?\C-d
                              ?\" ?, return ?\" ?d ?e ?s ?c ?r ?i ?p ?t ?i ?o ?n ?\" ?:
                              ?\S- ?\" ?T ?h ?i ?s ?  ?i ?s ?  ?m ?y ?  ?d ?e ?s ?c ?r ?t
                              ?i ?p backspace backspace backspace ?i ?p ?t ?i ?o ?n ?\"
                              return ?\} ?\C-x ?\C-s ?\C-x ?k return ?n ?n] 0 "%d"))

;;;; Package initializations

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)
(require 'init-etc)

(ensure-all-packages-installed)

(require 'init-gui)
(require 'init-treesit)

(require 'winfast)
(define-key global-map (kbd "M-o") #'other-window)
(define-key global-map (kbd "M-O") #'winfast-mode)

(require 'init-alert)

(require 'init-aggressive-indent)
(require 'init-browse-kill-ring)
(require 'init-calibre)
;; (require 'init-bs)
(require 'init-chronos)
(require 'init-command-log)
(require 'init-company)
(require 'init-consult)
(require 'init-diminish)
(require 'init-dired)
(require 'init-dired-toggle)
(require 'init-ediff)
(require 'init-emms)
(require 'init-erc)
(require 'init-eshell)
(require 'init-expand-region)
(require 'init-flycheck)
(require 'init-graphviz-dot-mode)
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
(require 'init-org)
(require 'init-org-alert)
(require 'init-org-crypt)
(require 'init-org-modern)
(require 'init-org-present)
(require 'init-origami)
(require 'init-pdf)
(require 'init-plantuml)
(require 'init-prism)
(require 'init-recentf)
(require 'init-restclient)
(require 'init-selected)
(require 'init-slime)
(require 'init-smartparens)
(require 'init-transpose-frame)
(require 'init-which-key)
(require 'init-xref)
(require 'init-yasnippet)

;; Languages
(require 'init-clojurescript)
(require 'init-go)
(require 'init-javascript)
(require 'init-kotlin)
(require 'init-nix)
(require 'init-scheme)
(require 'init-solidity)
(require 'init-typescript)

(with-eval-after-load 'init-org
  (require 'init-org-modern))
(with-eval-after-load 'init-org
  (require 'init-org-roam))

(provide 'init)
;;; init.el ends here
