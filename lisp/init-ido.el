;;; init-ido.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package ido.

;;; Code:

(require 'ido)

;; Enable flexible string matching
(setq ido-enable-flex-matching 1)

;; Don't ask for confirmation for new files/buffers
(setq ido-create-new-buffer 'always
      confirm-nonexistent-file-or-buffer nil)

;; Maximum number of prospect list
(setq ido-max-prospects 10)

;; Allow referring to virtual/past buffers
(setq ido-use-virtual-buffers t)

;; Don't show the Completion buffer on TAB
(setq ido-completion-buffer nil)

;; Delay time when merging the working directories to the choices
(setq ido-auto-merge-delay-time 0.3)

;; Don't expand the minibuffer
(setq ido-max-window-height 1)
(add-hook 'ido-minibuffer-setup-hook
	  (lambda ()
	    (visual-line-mode nil)))

(setq ido-decorations
      '(" { "
	" } "
	" | "
	" | ..."
	"["
	"]"
	" [No match]"
	" [Matched]"
	" [Not readable]"
	" [Too big]"
	" [Confirm]"))

(ido-everywhere 1)
(ido-mode 1)

;; Add ido-style completion in non-file/buffer prompts
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; Use package-provided ido support
(setq magit-completing-read-function 'magit-ido-completing-read)
(setq gnus-completing-read-function 'gnus-ido-completing-read)
(setq ess-use-ido t)

;; Better ido-style M-x
(require 'amx)
(amx-mode 1)

(provide 'init-ido)
;;; init-ido.el ends here
