;;; init.el --- Nicolai's Emacs init file.

;;; Commentary:

;; This contains my personal Emacs settings.

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq my-scratch-message "* (my/find-init-file)")
(setq inhibit-startup-screen t
      initial-scratch-message my-scratch-message
      initial-major-mode 'org-mode
      visible-bell t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(column-number-mode t)
(show-paren-mode t)

(setq my-font "DejaVu Sans Mono-10:weight=book:width=regular")

(set-face-attribute 'default t :font my-font)
(add-to-list 'default-frame-alist `(font . ,my-font))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(background-color . "White"))
(add-to-list 'default-frame-alist '(foreground-color . "gray25"))
(set-face-background 'region "RosyBrown2")

;; Where to store backup and autosave files
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosaves/") t)))

(fset 'yes-or-no-p 'y-or-n-p)

;; Disable transient mode
(transient-mark-mode -1)

;; Override just-one-space
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Allow C-SPC to continue popping the mark after C-u C-SPC
(setq set-mark-command-repeat-pop 1)

;; Other keybinds
(global-set-key (kbd "C-x C-S-c") 'save-buffers-kill-emacs)

(defun my/find-init-file ()
  "Find my Emacs init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun my/sudo-find-alternate-file ()
  "Find this file as sudo."
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

;; ace-window
(require 'ace-window)
(setq aw-dispatch-always t)
(global-set-key (kbd "C-;") 'ace-window)
(add-to-list 'aw-dispatch-alist '(? (lambda () (other-window 1))))

;; AUCTeX
(setq TeX-auto-save t
      TeX-parse-self t)
;; enable for multi-file document structure
;; (setq-default TeX-master nil)

(pdf-tools-install)
(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))

;; avy
(require 'avy)
(global-set-key (kbd "C-'") 'avy-goto-char-timer)
(setq avy-timeout-seconds 0.3)

;; avy-zap
(require 'avy-zap)
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)
(global-set-key (kbd "M-Z") 'avy-zap-up-to-char-dwim)

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; change-inner
(require 'change-inner)
(global-set-key (kbd "C-c i") 'change-inner)
(global-set-key (kbd "C-c o") 'change-outer)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; company-lsp
(require 'company-lsp)
(push 'company-lsp company-backends)
(setq company-lsp-cache-candidates t
      company-lsp-enable-recompletion t
      company-lsp-enable-snippet t)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-after-open-hook 'yas-minor-mode))

;; counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x r b") 'counsel-bookmark)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h C-l") 'counsel-find-library)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h C-u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

;; counsel-projectile
(with-eval-after-load 'projectile
  (require 'counsel-projectile)
  (counsel-projectile-mode 1))

;; discover-my-major
(require 'discover-my-major)
(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)

;; edit-server
(require 'edit-server)
(edit-server-start)

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;; erc (Emacs IRC)
(require 'erc)
(erc-dcc-mode 1)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; flycheck
(require 'flycheck)
;(add-hook 'after-init-hook 'global-flycheck-mode)
(setq-default flycheck-idle-change-delay 1.0)
(setq-default flycheck-check-syntax-automatically
	      '(save idle-change mode-enabled ))

;; flycheck-kotlin
(with-eval-after-load 'flycheck
    (require 'flycheck-kotlin)
    (flycheck-kotlin-setup)
    (add-hook 'kotlin-mode-hook 'flycheck-mode))

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "nas")))

;; ido-mode
;; (require 'ido)
;; (ido-everywhere t)
;; (ido-mode t)
;; (ido-vertical-mode t)
;; (setq ido-enable-flex-matching t
;;       do-vertical-show-count t)

;; ivy
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t
      ivy-wrap t
      ivy-height 10)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; kotlin-mode
(add-hook 'kotlin-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (add-hook 'before-save-hook
		      'delete-trailing-whitespace nil t)))

;; lsp-mode
(require 'lsp-mode)
;; (add-hook 'kotlin-mode-hook #'lsp-prog-major-mode-enable)

;; lsp-ui
(with-eval-after-load 'lsp-mode
  (require 'lsp-ui)
  (add-hook 'lsp-after-open-hook 'lsp-ui-mode))

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; modalka
(require 'modalka)
(global-set-key (kbd "C-z") 'modalka-mode)
(modalka-define-kbd "c i" "C-c i")
(modalka-define-kbd "c o" "C-c o")
(modalka-define-kbd "n" "C-n")
(modalka-define-kbd "o" "C-o")
(modalka-define-kbd "p" "C-p")
(modalka-define-kbd "e" "C-e")
(modalka-define-kbd "a" "C-a")
(modalka-define-kbd "f" "M-f")
(modalka-define-kbd "l" "C-l")
(modalka-define-kbd "b" "M-b")
(modalka-define-kbd "d" "M-d")
(modalka-define-kbd "u" "C-u")
(modalka-define-kbd "=" "C-=")
(modalka-define-kbd "SPC" "C-SPC")
(modalka-define-kbd "0" "C-0")
(modalka-define-kbd "1" "C-1")
(modalka-define-kbd "2" "C-2")
(modalka-define-kbd "3" "C-3")
(modalka-define-kbd "4" "C-4")
(modalka-define-kbd "5" "C-5")
(modalka-define-kbd "6" "C-6")
(modalka-define-kbd "7" "C-7")
(modalka-define-kbd "8" "C-8")
(modalka-define-kbd "9" "C-9")

(add-hook 'modalka-mode-hook
	  ;; Change cursor color on emacs/modalka mode
	  (lambda ()
	    (if modalka-mode
		(progn
		  (setq prev-cursor-color
			(cdr (assoc 'cursor-color (frame-parameters))))
		  (set-cursor-color "deep sky blue"))
	      (set-cursor-color prev-cursor-color))))

;; org-mode
(require 'org)
(setq org-startup-indented t
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files '("~/org/notes.org"
			 "~/org/personal.org")
      org-refile-targets '((nil :maxlevel . 3)
			   (org-agenda-files :maxlevel . 2))
      org-capture-templates `(("t" "TODO" entry (file+headline org-default-notes-file "Tasks")
			       "* TODO %?\nCreated %U\n")
			      ("T" "TODO - Annotated" entry (file+headline org-default-notes-file "Tasks")
			       "* TODO %?\n%U\n%a\n")
			      ("n" "Note" entry (file+headline org-default-notes-file "Notes")
			       "* %?\n%U\n")
			      ("i" "Idea" entry (file+headline org-default-notes-file "Ideas")
			       "* %?\n%U\n")
			      ("b" "Bookmark" entry (file+headline ,(concat org-directory "/bookmarks.org") "Web bookmarks")
			       "* [[%x][%?]] %^g\n%U")))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)

(setq org-tag-alist '(("@dev" . ?d)
		      ("@idea" . ?i)
		      ("@learn" . ?l)
		      ("@home" . ?h)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (local-unset-key (kbd "C-,"))
	    (local-unset-key (kbd "C-'"))
	    (add-hook 'before-save-hook
		      'delete-trailing-whitespace nil t)))

;; origami
(require 'origami)
(define-key origami-mode-map (kbd "C-c f f") 'origami-toggle-node)
(define-key origami-mode-map (kbd "C-c f O") 'origami-open-all-nodes)
(define-key origami-mode-map (kbd "C-c f C") 'origami-close-all-nodes)
(global-origami-mode)

;; plantuml-mode
(setq plantuml-jar-path "/usr/share/java/plantuml.jar"
      plantuml-output-type "png")
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; projectile
(require 'projectile)
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; recentf
(require 'recentf)
(setq recentf-max-menu-items 25)
(recentf-mode 1)

;; smartparens
(require 'smartparens-config)
(add-hook 'prog-mode-hook (lambda ()
			    (smartparens-mode t)))

;; smex
;; (require 'smex)
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-S-x") 'smex-major-mode-commands)
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; which-key
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right-bottom)
(global-set-key (kbd "C-h C-b") 'which-key-show-major-mode)

;; winner-mode
(winner-mode t)

;; reader minor mode
(define-minor-mode reader-mode
  "Make a reader-friendly view by removing screen distractions
  and adding margins."
  :init-value nil
  :lighter " Reader"
  :global nil
  :group 'reader

  (if reader-mode
      (progn
	(writeroom-mode 1)
	(visual-line-mode 1))
    (progn
      (writeroom-mode -1)
      (visual-line-mode -1))))

;; winresize minor mode
(define-minor-mode winresize-mode
  "Toggle winresize mode.  When enabled, this makes resizing
  windows easier."
  :init-value nil
  :lighter " Win-Resize"
  :global t
  :group 'winresize
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-1") 'delete-other-window)
	    (define-key map (kbd "C-2") 'split-window-below)
	    (define-key map (kbd "C-3") 'split-window-right)
	    (define-key map (kbd "C-0") 'delete-window)
	    (define-key map (kbd "C-f") 'enlarge-window-horizontally)
	    (define-key map (kbd "C-b") 'shrink-window-horizontally)
	    (define-key map (kbd "C-n") 'enlarge-window)
	    (define-key map (kbd "C-p") 'shrink-window)
	    (define-key map (kbd "C--") 'shrink-window-if-larger-than-buffer)
	    (define-key map (kbd "C-=") 'balance-windows)
	    (define-key map (kbd "C-S-o") 'other-window)
	    (define-key map (kbd "<return>") 'winresize-mode)
	    (define-key map (kbd "<escape>") 'winresize-mode)
	    map))
(global-set-key (kbd "C-c w r") 'winresize-mode)

;; xmodmap mode
;; From: https://www.emacswiki.org/emacs/XModMapMode
(define-generic-mode 'xmodmap-mode
      '(?!)
      '("add" "clear" "keycode" "keysym" "pointer" "remove")
      nil
      '("[xX]modmap\\(rc\\)?\\'")
      nil
      "Simple mode for xmodmap files.")

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;; init.el ends here
