;;; init.el --- Nicolai's Emacs init file.

;;; Commentary:

;; This contains my personal Emacs settings.

;;; Code:

(setq original-gc-cons-percentage gc-cons-percentage
      higher-gc-cons-percentage 0.6)

(defun increase-gc-cons-percentage ()
  "Set a higher value for gc-cons-percentage to prevent garbage
collection.  Use revert-gc-cons-percentage to restore the value."
  (setq gc-cons-percentage higher-gc-cons-percentage))

(defun revert-gc-cons-percentage ()
  "Restore gc-cons-percentage to its original value."
  (setq gc-cons-percentage original-gc-cons-percentage))

(increase-gc-cons-percentage)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)

(require 'init-browse-kill-ring)
(require 'init-discover-my-major)
(require 'init-erc)
(require 'init-expand-region)
;; (require 'init-ibuffer)
(require 'init-ivy)
(require 'init-js2-mode)
(require 'init-latex)
(require 'init-magit)
(require 'init-org)
(require 'init-origami)
(require 'init-plantuml)
(require 'init-which-key)

(require 'init-gui)
(require 'init-etc)

(winner-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Where to store backup and autosave files
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups/")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosaves/") t)))

;; Allow C-SPC to continue popping the mark after C-u C-SPC
(setq-default set-mark-command-repeat-pop 1)

(global-set-key (kbd "C-x C-S-c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Set up customize
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'minibuffer-setup-hook #'increase-gc-cons-percentage)
(add-hook 'minibuffer-exit-hook #'revert-gc-cons-percentage)

(add-hook 'after-init-hook #'revert-gc-cons-percentage)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message (concat "Emacs startup took " (emacs-init-time) " with " (number-to-string gcs-done) " GCs."))))

(provide 'init)
;;; init.el ends here
