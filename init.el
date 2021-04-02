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

(add-hook 'after-init-hook #'server-start)

;; Always enable the following functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(winner-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Do not ask to follow symlinks; always follow
(setq vc-follow-symlinks t)

;; Save bookmarks immediately
(setq bookmark-save-flag 1)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers 1)
(minibuffer-depth-indicate-mode 1)

;; Where to store backup and autosave files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups/")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosaves/") t)))

;; Allow C-SPC to continue popping the mark after C-u C-SPC
(setq-default set-mark-command-repeat-pop 1)

;; Show N lines of text on top/bottom when scrolling
(setq-default scroll-margin 2)

;; Display long lines as is
(setq-default truncate-lines t)

;; Show paren matches instantly
(setq show-paren-delay 0)

;; Tabs and spaces handling
(setq-default tab-width 4)
(add-hook 'shell-mode-hook #'indent-tab-width-8)
(add-hook 'emacs-lisp-mode-hook #'indent-tab-width-8)
(add-hook 'kotlin-mode-hook #'indent-spaces)

;; Don't ask when reverting for the following regex
(setq revert-without-query '("^.*\\.pdf$"))

(defun turn-off-indent-tabs-mode () (setq indent-tabs-mode nil))
(add-hook 'emacs-lisp-mode-hook #'turn-off-indent-tabs-mode)
(add-hook 'scheme-mode-hook #'turn-off-indent-tabs-mode)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Line display
(global-set-key (kbd "C-c t l") #'toggle-truncate-lines)
(global-set-key (kbd "C-c h l") #'hl-line-mode)

;; Allow repeating keys for previous
(defun previous-buffer-repeatable ()
  "Call `previous-buffer' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'previous-buffer))

(defun next-buffer-repeatable ()
  "Call `next-buffer' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'next-buffer))

(global-set-key (kbd "C-x <left>") #'previous-buffer-repeatable)
(global-set-key (kbd "C-x <right>") #'next-buffer-repeatable)

;; Other key bindings
(global-set-key (kbd "C-x C-S-c") #'save-buffers-kill-emacs)
(global-set-key (kbd "M-SPC") #'cycle-spacing)
(global-set-key (kbd "C-x C-m") (key-binding (kbd "M-x")))
(global-set-key (kbd "C-z") #'repeat)

;; Move facemenu-keymap to another binding
(define-key global-map (kbd "M-o") nil)
(define-key global-map (kbd "C-c f m") 'facemenu-keymap)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message (concat "Emacs startup took " (emacs-init-time) " with " (number-to-string gcs-done) " GCs."))))

;;;; Load Customize file

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;; Package initializations

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)
(ensure-all-packages-installed)

(require 'init-gui)
(require 'init-etc)
(require 'winfast)

(require 'init-aggressive-indent)
(require 'init-browse-kill-ring)
(require 'init-bs)
(require 'init-command-log)
(require 'init-company)
(require 'init-diminish)
(require 'init-dired)
(require 'init-dired-toggle)
(require 'init-discover-my-major)
(require 'init-ediff)
(require 'init-erc)
(require 'init-eshell)
(require 'init-expand-region)
(require 'init-flycheck)
(require 'init-graphviz-dot-mode)
(require 'init-hippie-expand)
;; (require 'init-ibuffer)
(require 'init-icomplete)
;; (require 'init-ido)
(require 'init-imenu-list)
(require 'init-isearch)
;; (require 'init-ivy)
(require 'init-js2-mode)
(require 'init-json-navigator)
(require 'init-latex)
(require 'init-magit)
(require 'init-multiple-cursors)
(require 'init-native-complete)
(require 'init-nov)
(require 'init-org)
(require 'init-origami)
(require 'init-pdf)
(require 'init-plantuml)
(require 'init-prism)
(require 'init-recentf)
(require 'init-restclient)
(require 'init-smartparens)
(require 'init-transpose-frame)
(require 'init-which-key)

(require 'init-clojurescript)
(require 'init-kotlin)

(provide 'init)
;;; init.el ends here
