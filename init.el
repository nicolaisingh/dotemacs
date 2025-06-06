;;; init.el --- Nicolai Singh's .emacs -*- lexical-binding: t -*-

;;; Commentary:

;; This contains my personal Emacs settings.

;;; Code:

(defmacro skipped (&rest body)
  "Discard BODY."
  (declare (indent nil))
  nil)

;;;; Startup

(setq original-gc-cons-percentage gc-cons-percentage
      higher-gc-cons-percentage 0.6
      gc-idle-timer nil)

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

(increase-gc-cons-percentage)
(start-gc-idle-timer)
(add-hook 'after-init-hook (lambda ()
                             (revert-gc-cons-percentage)
                             (garbage-collect)))


;;;; General settings

(setq auto-hscroll-mode 'current-line
      auto-save-interval 50
      auto-save-timeout 3
      history-delete-duplicates t
      scroll-margin 0
      tab-width 4
      truncate-lines t
      user-full-name "Nicolai Singh")

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;; Source code
(setq find-function-C-source-directory "/run/current-system/sw/share/emacs/source/src")
(visit-tags-table (format "/run/current-system/sw/share/emacs/%s/lisp/TAGS" emacs-version))

(setq enable-recursive-minibuffers 1)
(minibuffer-depth-indicate-mode 1)

;; Don't wrap minibuffer entries
(add-hook 'minibuffer-setup-hook (lambda () (setq truncate-lines t)))

;; Tabs and spaces handling
(defmacro define-set-indent-tab-width-fn (width)
  "Macro to define a simple function to set `tab-width' to WIDTH."
  `(defun ,(intern (format "set-indent-tab-width-%d" width)) ()
     (interactive)
     ,(format "Set `tab-width' to %d." width)
     (setq tab-width ,width)))
(define-set-indent-tab-width-fn 2)
(define-set-indent-tab-width-fn 4)
(define-set-indent-tab-width-fn 8)

(add-hook 'shell-mode-hook #'set-indent-tab-width-8)
(add-hook 'emacs-lisp-mode-hook #'set-indent-tab-width-2)

;;; abbrev

(setq abbrev-file-name (expand-file-name "abbrev-defs.el" user-emacs-directory))

;;; bookmark

(setq bookmark-save-flag 1)

;;; custom

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; facemenu

;; Move facemenu to another binding
(require 'facemenu)
(keymap-global-unset "M-o")
(keymap-global-set "C-c f m" #'facemenu-keymap)

;;; files

(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save-files/") t))
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
      confirm-kill-emacs #'yes-or-no-p
      large-file-warning-threshold nil
      require-final-newline t
      revert-without-query '("^.*\\.pdf$"))

;;; grep

(require 'grep)
(defun rgrep-dired (regexp files)
  "Run `rgrep' in the current directory to match REGEXP in FILES."
  (interactive "srgrep regexp: \nsfilename wildcard (blank for `*'): ")
  (let* ((case-fold-search (if current-prefix-arg nil t))
         (files (if (equal files "") "*" files)))
    (when (eq grep-find-template nil)
      (grep-compute-defaults))
    (rgrep regexp files (dired-current-directory) nil)))

(defun lgrep-dired (regexp files)
  "Run `lgrep' in the current directory to match REGEXP in FILES."
  (interactive "slgrep regexp: \nsfilename wildcard (blank for `*'): ")
  (let* ((case-fold-search (if current-prefix-arg nil t))
         (files (if (equal files "") "*" files)))
    (when (eq grep-find-template nil)
      (grep-compute-defaults))
    (lgrep regexp files (dired-current-directory) nil)))

(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "node_modules")))

(require 'dired)
(keymap-set dired-mode-map "C-c m g r" #'rgrep-dired)
(keymap-set dired-mode-map "C-c m g l" #'lgrep-dired)

;;; help

(setq help-window-select t)

;;; indent

(setq tab-always-indent 'complete)

;;; minibuffer

(setq completions-format 'one-column)

;;; novice

(setq disabled-command-function nil)

;;; paren

(setq show-paren-delay 0
      show-paren-style 'parenthesis
      show-paren-when-point-inside-paren nil)

;;; register

(setq register-preview-delay 0)

;;; repeat

(repeat-mode 1)

(with-eval-after-load 'org-roam
  (defvar-keymap org-roam-dailies-repeat-map
    :repeat t
    "." #'org-roam-dailies-goto-next-note
    "," #'org-roam-dailies-goto-previous-note))

(with-eval-after-load 'calendar
  (defvar-keymap calendar-mode-repeat-map
    :repeat t
    "]" #'calendar-forward-year
    "[" #'calendar-backward-year))

(with-eval-after-load 'howm
  (defvar-keymap howm-mode-repeat-map
    :repeat t
    "n" #'action-lock-goto-next-link
    "p" #'action-lock-goto-previous-link
    "N" #'howm-next-memo
    "P" #'howm-previous-memo
    "F" #'howm-first-memo
    "L" #'howm-last-memo))

;;; saveplace

(save-place-mode 1)

;;; simple

(setq eval-expression-print-length nil  ; don't truncate when evaluating exprs
      ;; eval-expression-print-level nil
      kill-do-not-save-duplicates t
      kill-whole-line t
      next-line-add-newlines t
      save-interprogram-paste-before-kill t
      set-mark-command-repeat-pop t)

(defun no-indent-tabs-mode ()
  "Turn of `indent-tabs-mode' (i.e. indent using spaces)."
  (indent-tabs-mode -1))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'emacs-lisp-mode-hook #'no-indent-tabs-mode)
(add-hook 'kotlin-mode-hook #'no-indent-tabs-mode)
(add-hook 'scheme-mode-hook #'no-indent-tabs-mode)
(add-hook 'typescript-ts-mode-hook #'no-indent-tabs-mode)

;;; startup

(setq user-mail-address "nicolaisingh@pm.me"
      initial-scratch-message ""
      initial-major-mode 'fundamental-mode)

;;; tab-bar

(setq tab-bar-tab-hints t)

(defun my-select-tab-or-other-window (arg)
  (interactive "P")
  (cond
   ((eq 0 arg) (tab-recent))
   ((numberp arg) (tab-bar-select-tab arg))
   (t (other-window 1))))

(keymap-global-set "C-x t T" #'tab-bar-mode)
(keymap-global-set "C-`" #'my-select-tab-or-other-window)

;;; vc

(setq vc-follow-symlinks t)

;;; window

(setq same-window-regexps '("^magit: .*$"
                            "^magit-status: .*$"))

;;; winner

(winner-mode 1)


;;;; Keybinding prefixes

(define-prefix-command 'my-ctl-c-D-map)
(define-prefix-command 'my-ctl-c-M-map)
(define-prefix-command 'my-ctl-c-b-map)
(define-prefix-command 'my-ctl-c-c-map)
(define-prefix-command 'my-ctl-c-d-map)
(define-prefix-command 'my-ctl-c-e-map)
(define-prefix-command 'my-ctl-c-f-map)
(define-prefix-command 'my-ctl-c-g-map)
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

(keymap-global-set "C-c D" 'my-ctl-c-D-map)
(keymap-global-set "C-c M" 'my-ctl-c-M-map)
(keymap-global-set "C-c b" 'my-ctl-c-b-map)
(keymap-global-set "C-c c" 'my-ctl-c-c-map)
(keymap-global-set "C-c d" 'my-ctl-c-d-map)
(keymap-global-set "C-c e" 'my-ctl-c-e-map)
(keymap-global-set "C-c f" 'my-ctl-c-f-map)
(keymap-global-set "C-c g" 'my-ctl-c-g-map)
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


;;;; Minor modes

(define-minor-mode reader-mode
  "Make a reader-friendly view by removing screen distractions and adding margins."
  :init-value nil
  :lighter " Reader"
  :global nil
  :group 'reader

  (let ((enabled (if reader-mode t -1)))
    (writeroom-mode enabled)
    (visual-line-mode enabled)))

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.

From https://www.emacswiki.org/emacs/XModMapMode")

(define-generic-mode miranda-mode
  '("||")
  '("where" "if" "otherwise")
  nil
  nil
  nil
  "Generic major mode for the Miranda programming language.")


;;;; Functions

(defun my-sort-init-file ()
  "Sort the init file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Packages
    (re-search-forward "^;;;; Package loads$")
    (forward-page)
    (let ((start (point)))
      (point-marker)
      (end-of-buffer)
      (backward-page)
      (sort-pages nil start (point)))))

(defun find-init-file ()
  "Find my Emacs init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun find-scratch-buffer ()
  "Find the *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun sudo-find-file (file-or-buffer)
  "Find local file/directory FILE-OR-BUFFER as sudo."
  (let ((sudomethod (if (< emacs-major-version 27) "/sudo::" "/sudoedit::")))
    (find-file (concat sudomethod file-or-buffer))))

(defun sudo-find-alternate-file ()
  "Alternate find a file/directory as sudo."
  (interactive)
  (require 'tramp)
  (let ((current-buffer (cond
                         ((derived-mode-p 'dired-mode) (dired-current-directory))
                         ((buffer-file-name) (buffer-file-name))
                         (t (error "Buffer is not visiting a file")))))
    (if (tramp-tramp-file-p current-buffer)
        ;; Use tramp to sudo
        (let ((sudomethod "sudo")
              (sudouser "root")
              (vec (tramp-dissect-file-name tramp-file-name)))
          (find-alternate-file (tramp-make-tramp-file-name
                                sudomethod
                                sudouser
                                (tramp-file-name-domain vec)
                                (tramp-file-name-host vec)
                                nil ;; PORT
                                (tramp-file-name-localname vec)
                                (tramp-make-tramp-hop-name vec))))
      ;; Emacs 27 added connection method `/sudoedit' for security
      ;; reasons.  Use if available; otherwise, use `/sudo'.
      (let ((sudomethod (if (< emacs-major-version 27) "/sudo::" "/sudoedit::")))
        (find-alternate-file (concat sudomethod current-buffer))))))

(defun indent-using-tabs-and-fixup ()
  (interactive)
  (indent-tabs-mode 1)
  (tabify (point-min) (point-max)))

(defun indent-using-spaces-and-fixup ()
  (interactive)
  (indent-tabs-mode -1)
  (untabify (point-min) (point-max)))

(defun toggle-line-and-column-numbers ()
  "Toggle `line-number-mode' and `column-number-mode'."
  (interactive)
  (line-number-mode 'toggle)
  (column-number-mode 'toggle))

(defun benchmark-this (repetitions)
  "Time the execution of the last sexp or the region if active.
Specify a prefix argument to perform the execution REPETITIONS
times."
  (interactive "p")
  (let ((form (if (use-region-p)
                  (read (concat "(progn "
                                (buffer-substring-no-properties
                                 (region-beginning)
                                 (region-end))
                                ")"))
                (pp-last-sexp))))
    (if (not form)
        (error "No form found")
      (message "Form: %S\nBenchmark: %S"
               form
               (eval `(benchmark-run ,repetitions ,form))))))

(defun profiler-toggle ()
  (interactive)
  (require 'profiler)
  (if (profiler-running-p)
      (profiler-stop)
    (profiler-start 'cpu+mem)))

(defun repeatkey-repeatable-call (f &optional other-bindings)
  "Call function F interactively, then allow the last key used to
  repeat the call, similar to C-x z z z in `repeat'."
  (setq repeatkey-last-command f)
  (let ((repeat-char last-command-event))
    (call-interactively f)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector repeat-char)
                   (lambda ()
                     (interactive)
                     (repeatkey-repeatable-call repeatkey-last-command)))
       (dolist (binding other-bindings)
         (define-key map (kbd (car binding)) (cdr binding)))
       map))))

(defun toggle-hscroll-mode ()
  "Toggle `auto-hscroll-mode' between t and 'current-line."
  (interactive)
  (let ((value (if (eq auto-hscroll-mode t) 'current-line t)))
    (setq auto-hscroll-mode value)
    (message "auto-hscroll-mode: %s" value)))

(defun delete-undo-history ()
  "Delete the undo history of this buffer."
  (interactive)
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun my-yank-to-other-window ()
  "Yank the current word or the region, if active, to the other window."
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word)))
        (separator (cond
                    ((equal current-prefix-arg '(4)) " ")
                    ((equal current-prefix-arg '(16)) "")
                    (t "\n"))))
    (save-excursion
      (set-buffer (window-buffer (next-window)))
      (insert separator text))))

(defun my-sanitize-string (str)
  (replace-regexp-in-string "[^[:alnum:]-]" "_" str))

(defun single-blank-lines-only ()
  (interactive)
  "Replace consecutive blank lines with single blank lines in the entire buffer."
  (replace-regexp-in-region "\n+$" "\n" (point-min) (point-max)))

(defun my-presorted-completion-table (completions)
  "Closure for achieving consistent sorting in completing-read."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (cycle-sort-function . ,#'identity)
                   (display-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))

(defun region-bytes (start end)
  "Return the number of bytes used by the region."
  (interactive "r")
  (message "Region has %d bytes"
           (- (bufferpos-to-filepos end 'exact)
              (bufferpos-to-filepos start 'exact))))

(defun my-sort-strings (reverse beg end)
  "Sort string literals in the region."
  (interactive "P\nr")
  (sort-regexp-fields reverse "\"[^\"]*\"" "\\&" beg end))

(keymap-global-set "C-c y o" #'my-yank-to-other-window)
(keymap-global-set "C-c i TAB" #'indent-using-tabs-and-fixup)
(keymap-global-set "C-c i SPC" #'indent-using-spaces-and-fixup)
(keymap-global-set "C-c D ." #'benchmark-this)
(keymap-global-set "C-c f #" #'sudo-find-alternate-file)
(keymap-global-set "C-c f s" #'find-scratch-buffer)
(keymap-global-set "C-c h s" #'toggle-hscroll-mode)


;;;; Post-startup

(defun my-other-keybindings ()
  (keymap-global-set "C-c D T" #'cancel-debug-on-entry)
  (keymap-global-set "C-c D V" #'cancel-debug-on-variable-change)
  (keymap-global-set "C-c D e" #'toggle-debug-on-error)
  (keymap-global-set "C-c D p p" #'profiler-toggle)
  (keymap-global-set "C-c D p r" #'profiler-report)
  (keymap-global-set "C-c D q" #'toggle-debug-on-quit)
  (keymap-global-set "C-c D t" #'debug-on-entry)
  (keymap-global-set "C-c D v" #'debug-on-variable-change)
  (keymap-global-set "C-c d l" #'dictionary-search)
  (keymap-global-set "C-c h l" #'hl-line-mode)
  (keymap-global-set "C-c l d" #'duplicate-line)
  (keymap-global-set "C-c v f" #'visual-line-fill-column-mode)
  (keymap-global-set "C-c w '" #'insert-pair)
  (keymap-global-set "C-c w <" #'insert-pair)
  (keymap-global-set "C-c w [" #'insert-pair)
  (keymap-global-set "C-c w \"" #'insert-pair)
  (keymap-global-set "C-h C-k" #'describe-keymap)
  (keymap-global-set "C-h u f" #'find-library)
  (keymap-global-set "C-x B" #'bury-buffer)
  (keymap-global-set "C-x C-M-c" #'save-buffers-kill-emacs)
  (keymap-global-set "C-x C-m" (key-binding (kbd "M-x")))
  (keymap-global-set "C-x D" (lambda () (interactive) (dired "~")))
  (keymap-global-set "C-x K" #'kill-current-buffer)
  (keymap-global-set "C-x a /" #'unexpand-abbrev)
  (keymap-global-set "C-x M-x" #'switch-to-minibuffer)
  (keymap-global-set "C-z C-s" #'eshell-toggle)
  (keymap-global-set "C-z C-z" #'my-org-capture-inbox)
  (keymap-global-set "C-S-z" #'org-capture)
  (keymap-global-set "M-SPC" #'cycle-spacing)
  (keymap-global-unset "C-h C-c")
  (keymap-global-unset "C-h C-f")
  (keymap-global-set "C-c j" (lambda ()
                               (interactive)
                               (browse-url (concat "https://JIRA.atlassian.net/browse/" (symbol-name (symbol-at-point)))))))
(add-hook 'emacs-startup-hook #'my-other-keybindings)

(defun message-init-time ()
  "Report Emacs init time."
  (message "Emacs init time: %s with %d GCs" (emacs-init-time) gcs-done))
(add-hook 'emacs-startup-hook #'message-init-time)

;; (add-hook 'after-init-hook #'server-start)


;;;; Package loads

(require 'mode-local)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;;; 00 straight

(setq straight-host-usernames '((github . "nicolaisingh"))
      straight-profiles `((nil . ,(expand-file-name "lisp/straight/versions/default.el"
                                                    user-emacs-directory)))
      straight-vc-git-default-clone-depth 'full)
(load (expand-file-name "packages.el" user-emacs-directory))

(keymap-global-set "C-h u r" #'straight-get-recipe)
(keymap-global-set "C-h u w" #'straight-visit-package-website)


;;; 01 exec-path-from-shell

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;; 02 early-init

(when (< emacs-major-version 27)
  ;; Manually load early-init.el for older versions of Emacs
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (when (file-exists-p early-init-file)
      (load early-init-file))))


;;; adoc-mode / asciidoc

(require 'adoc-mode)

(setq adoc-default-title-type 1
      adoc-default-title-sub-type 1)

(defun my-adoc-insert-title-2 ()
  "Insert a new title line."
  (interactive)
  (let ((title-line-p (save-excursion
                        (beginning-of-line)
                        (re-search-forward "^=+ " (pos-eol) t))))
    (cond
     (title-line-p
      (forward-line 1)
      (open-line 1))
     (t (forward-paragraph 1)
        (insert "\n")))
    (tempo-template-adoc-title-2)))

(defun my-adoc-promote ()
  "Increase a header's level."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (not (re-search-forward "^=+ " (pos-eol) t))
        (message "Not a title")
      (beginning-of-line)
      (insert "="))))

(defun my-adoc-demote ()
  "Decrease a header's level."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (not (re-search-forward "^==+ " (pos-eol) t))
        (message "Cannote demote any further")
      (beginning-of-line)
      (delete-char 1))))

(keymap-unset adoc-mode-map "C-c C-t")
(keymap-set adoc-mode-map "C-c C-b" #'tempo-template-adoc-bold)
(keymap-set adoc-mode-map "C-c C-d" #'my-adoc-demote)
(keymap-set adoc-mode-map "C-c C-p" #'my-adoc-promote)
(keymap-set adoc-mode-map "C-c C-t" #'my-adoc-insert-title-2)


;;; aggressive-indent

(require 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojurescript-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'aggressive-indent-mode)
(add-hook 'js2-mode-hook #'aggressive-indent-mode)
(keymap-global-set "C-c i A" #'aggressive-indent-mode)

;; Don't aggressively indent comment lines in the edebug eval list buffer
(add-hook 'edebug-eval-mode-hook
          (lambda () (interactive) (aggressive-indent-mode -1)))


;;; aider

(require 'aider)

(setq aider-todo-keyword-pair '("AI!" . "comment line ending with string: AI!"))

(defun my-aider-run-aider (orig-fn edit-args)
  "Call `aider-run-aider' with needed args and logging disabled."
  (interactive "P")
  (setenv "OPENAI_API_KEY" (auth-source-pick-first-password :host "api.openai.com"))
  (setenv "DEEPSEEK_API_KEY" (auth-source-pick-first-password :host "api.deepseek.com"))
  (let ((aider-args `(;; DeepSeek
                      ;; "--model" "deepseek/deepseek-chat"
                      "--model" "deepseek/deepseek-coder"
                      ;; "--model" "deepseek/deepseek-reasoner"

                      ;; OpenAI
                      ;; "--model" "o3-mini"

                      ;; Other options
                      "--no-auto-commits"
                      "--no-auto-lint"
                      "--no-analytics"
                      "--yes-always"))
        (message-log-max nil)
        (inhibit-message t))
    (funcall orig-fn)))
(advice-add 'aider-run-aider :around #'my-aider-run-aider)

(keymap-global-set "C-c l a" #'aider-transient-menu)


;;; alert

(require 'alert)
(setq alert-fade-time 10)

(alert-define-style 'org-alert-email
                    :title "org-alert email"
                    :notifier
                    (lambda (info)
                      (save-window-excursion
                        (let ((tag (concat "["
                                           (plist-get info :title)
                                           "] "))
                              (body (plist-get info :message))
                              (title-body-length 40)
                              (message-interactive nil))
                          (message-mail "nicolaisingh+org-alert@protonmail.com"
                                        (concat tag (if (length> body title-body-length)
                                                        (concat (substring body 0 title-body-length) "...")
                                                      body)))
                          (message-goto-body)
                          (insert (format "[From %s]\n"
                                          (file-name-base (buffer-name (plist-get info :buffer)))))
                          (insert body)
                          (insert (format "\n\n-----\nAlert time: %s"
                                          (format-time-string "%Y-%m-%d %H:%M:%S")))
                          (message-send-and-exit)))))

(alert-define-style 'legacy-log
                    :title "Log to *Alerts* buffer (legacy)"
                    :notifier
                    (lambda (info)
                      (let* ((mes (plist-get info :message))
                             (sev (plist-get info :severity))
                             (len (length mes)))
                        (alert-legacy-log-notify mes sev len))))


;;; avy

(require 'avy)
(setq avy-timeout-seconds 0.4
      avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
(keymap-global-set "C-z C-v" #'avy-goto-char-timer)


;;; browse-kill-ring

(require 'browse-kill-ring)
(keymap-global-set "C-c b k" #'browse-kill-ring)


;;; bs

(skipped
 (require 'bs)

 (defun bs-must-show-modes (buf)
   "Return non-nil for buffers matching specific modes.  Used for
the configuration 'files-plus-some-buffers-and-modes."
   (let ((major-mode (buffer-local-value 'major-mode buf)))
     (memq major-mode '(term-mode
                        shell-mode
                        eshell-mode
                        fundamental-mode))))

 (defun bs-dont-show-modes (buf)
   "Return non-nil for buffers which should not be shown."
   (or (bs-visits-non-file buf)
       (let ((major-mode (buffer-local-value 'major-mode buf)))
         (memq major-mode '(dired-mode)))))

 (defun bs-not-dired-mode-p (buf)
   "Return non-nil for dired buffers."
   (not (let ((major-mode (buffer-local-value 'major-mode buf)))
          (memq major-mode '(dired-mode)))))

 ;; Show only files, some buffers and modes
 (add-to-list 'bs-configurations '("default--files-plus-some-buffers-and-modes"
                                   "^\\(\\*scratch\\*\\|test\\)$"
                                   bs-must-show-modes
                                   "^\\(\\*Ilist\\*\\|\\*Messages\\*\\)$"
                                   bs-dont-show-modes
                                   bs-sort-buffer-interns-are-last))

 ;; Show only dired buffers
 (add-to-list 'bs-configurations '("dired-only"
                                   nil
                                   nil
                                   nil
                                   bs-not-dired-mode-p
                                   nil))

 ;; Custom size column based on file size, not buffer size
 (defun bs--get-filesize-string (_start-buffer _all-buffers)
   "Return file size of the current buffer for Buffer Selection Menu."
   (let* ((attributes (file-attributes (buffer-name)))
          (filesize (if attributes (file-attribute-size attributes) 0))
          (a-megabyte (* 1024 1024)))
     (if (>= filesize a-megabyte)
         (concat (format "%.01f" (/ (float filesize) a-megabyte)) "M")
       (concat (format "%.01f" (/ (float filesize) 1024)) "K"))))
 (defun bs--sort-by-filesize (b1 b2)
   (let* ((b1-attributes (file-attributes (buffer-file-name b1)))
          (b2-attributes (file-attributes (buffer-file-name b2)))
          (b1-filesize (if b1-attributes (file-attribute-size b1-attributes) 0))
          (b2-filesize (if b2-attributes (file-attribute-size b2-attributes) 0)))
     (< b1-filesize b2-filesize)))

 (defun bs-set-configuration-and-refresh (config-name)
   (bs-set-configuration config-name)
   (bs--redisplay t)
   (bs-message-without-log config-name))

 (defun my-bs-config ()
   (keymap-set bs-mode-map "0" (lambda ()
                                 (interactive)
                                 (apply #'bs-set-configuration-and-refresh
                                        '("default--files-plus-some-buffers-and-modes"))))
   (keymap-set bs-mode-map "1" (lambda ()
                                 (interactive)
                                 (apply #'bs-set-configuration-and-refresh
                                        '("dired-only"))))
   (keymap-set bs-mode-map "2" (lambda ()
                                 (interactive)
                                 (apply #'bs-set-configuration-and-refresh
                                        '("all"))))
   (keymap-set bs-mode-map "/" #'isearch-forward)
   (hl-line-mode)
   (set (make-local-variable 'scroll-conservatively) 101))

 (add-hook 'bs-mode-hook #'my-bs-config)

 (setq bs-default-configuration "default--files-plus-some-buffers-and-modes"
       bs-max-window-height 20
       bs-minimal-buffer-name-column 20
       bs-attributes-list '((""       1   1 left  bs--get-marked-string)
                            ("M"      1   1 left  bs--get-modified-string)
                            ("R"      2   2 left  bs--get-readonly-string)
                            ("Buffer" bs--get-name-length 10 left  bs--get-name)
                            (""       1   1 left  " ")
                            ;; ("Size"   8   8 right bs--get-size-string)
                            ;; (""       1   1 left  " ")
                            ("Size" 8   8 right bs--get-filesize-string)
                            (""       1   1 left  " ")
                            ("Mode"   12 12 right bs--get-mode-name)
                            (""       2   2 left  "  ")
                            ("File"   12 12 left  bs--get-file-name)
                            (""       2   2 left  "  "))
       bs-sort-functions '(("by name"     bs--sort-by-name     "Buffer" region)
                           ;; ("by size"     bs--sort-by-size     "Size"   region)
                           ("by filesize" bs--sort-by-filesize "Size"  region)
                           ("by mode"     bs--sort-by-mode     "Mode"   region)
                           ("by filename" bs--sort-by-filename "File"   region)
                           ("by nothing"  nil                  nil      nil)))

 (keymap-global-set "C-x C-b" #'bs-show))


;;; calendar

(require 'calendar)
(require 'year-calendar)
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)

(defun define-my-calendar-mark-org-headings-fn (buffer-name)
  (eval
   `(defun ,(intern (format "my-calendar-mark-headings-%s" buffer-name)) ()
      ,(format "Mark all dates that can be found at the end of each heading in %s." buffer-name)
      (let ((dates '())
            (bufname ,buffer-name))
        (save-excursion
          (set-buffer bufname)
          (goto-char (point-min))
          (while (re-search-forward (concat "^\\*+ .*" org-ts-regexp) nil t)
            (let ((context (org-element-context)))
              (when (eq (car context) 'timestamp)
                (let* ((year (plist-get (cadr context) :year-start))
                       (month (plist-get (cadr context) :month-start))
                       (day (plist-get (cadr context) :day-start))
                       (date `(,month ,day ,year)))
                  (push date dates))))))
        (dolist (date dates)
          (when (calendar-date-is-visible-p date)
            (calendar-mark-visible-date date)))))))

(defun my-calendar-mark-org-headings ()
  (interactive)
  (let ((bufname (buffer-name)))
    (if (memq (intern (concat "my-calendar-mark-headings-" bufname))
              calendar-today-visible-hook)
        (progn
          (remove-hook 'calendar-today-visible-hook (define-my-calendar-mark-org-headings-fn bufname))
          (remove-hook 'calendar-today-invisible-hook (define-my-calendar-mark-org-headings-fn bufname))
          (message "Will not mark headings for %s" bufname))
      (add-hook 'calendar-today-visible-hook (define-my-calendar-mark-org-headings-fn bufname))
      (add-hook 'calendar-today-invisible-hook (define-my-calendar-mark-org-headings-fn bufname))
      (message "Will mark headings for %s" bufname)
      (calendar))))


;;; calibre

(require 'calibre)
(setq calibre-libraries '(("library" . "~/calibre")))
(keymap-global-set "C-c L" #'calibre-library)


;;; cape

(require 'cape)
(keymap-global-set "C-c p &" #'cape-sgml)
(keymap-global-set "C-c p :" #'cape-emoji)
(keymap-global-set "C-c p \\" #'cape-tex)
(keymap-global-set "C-c p a" #'cape-abbrev)
(keymap-global-set "C-c p d" #'cape-dabbrev)
(keymap-global-set "C-c p e" #'cape-elisp-block)
(keymap-global-set "C-c p f" #'cape-file)
(keymap-global-set "C-c p h" #'cape-history)
(keymap-global-set "C-c p k" #'cape-keyword)
(keymap-global-set "C-c p l" #'cape-line)
(keymap-global-set "C-c p r" #'cape-rfc1345)
(keymap-global-set "C-c p s" #'cape-elisp-symbol)
(keymap-global-set "C-c p w" #'cape-dict)
;; (keymap-global-set "C-c p ^" #'cape-tex)
;; (keymap-global-set "C-c p _" #'cape-tex)
;; (keymap-global-set "C-c p p" #'completion-at-point) ; capf
;; (keymap-global-set "C-c p t" #'complete-tag)        ; etags

(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-line)
(add-to-list 'completion-at-point-functions #'cape-elisp-block)


;;; chatgpt-shell

(require 'chatgpt-shell)
(add-to-list
 'chatgpt-shell-system-prompts
 `("Family Travel Planning" . ,(string-join
                                '("The user is planning for a vacation with family and kid/s."
                                  "Suggest an itinerary that considers given events like flight schedules, commuting, walking, and even rest times."
                                  "Mention places that are known to be worth visiting and seeing."
                                  "Don't suggest activities and destinations that are not family-friendly."
                                  "Give me links to blogs or information that I can review."
                                  "The itinerary destinations preferrably should be sequenced such that it is travel-friendly."
                                  "If you will suggest a destination that is quite far from the others, mention it explicitly."
                                  "When you make the itinerary, include the travel times and an estimate of how long to stay on that area before going to the next one.")
                                "  "))
 t)

(setq chatgpt-shell-openai-key (lambda ()
                                 (auth-source-pick-first-password :host "api.openai.com"))
      chatgpt-shell-deepseek-key (lambda ()
                                   (auth-source-pick-first-password :host "api.deepseek.com"))
      chatgpt-shell-model-temperature 0
      chatgpt-shell-prompt-query-response-style 'shell
      chatgpt-shell-welcome-function nil
      ;; Set default system prompt
      chatgpt-shell-system-prompt (seq-position (map-keys chatgpt-shell-system-prompts) "Programming"))

(defun chatgpt-shell-rephrase-sentences ()
  (interactive)
  (chatgpt-shell-send-region-with-header
   (string-join
    '("I need you to improve and rephrase sentences without sounding too formal."
      "Give me only 3 improvements for each sentence, nothing else.") "  ")))

(defun chatgpt-shell-show-prompt ()
  (interactive)
  (let ((message-log-max nil))
    (if (not chatgpt-shell-system-prompt)
        (message "No prompt selected.")
      (message "%s" (cdr (nth chatgpt-shell-system-prompt
                              chatgpt-shell-system-prompts))))))

(defun chatgpt-shell-inline ()
  (interactive)
  (let ((chatgpt-shell-prompt-query-response-style 'inline))
    (call-interactively #'chatgpt-shell-prompt)))

(defun chatgpt-shell-other-buffer ()
  (interactive)
  (let ((chatgpt-shell-prompt-query-response-style 'other-buffer))
    (call-interactively #'chatgpt-shell-prompt)))

(keymap-global-set "C-c l C-SPC" #'chatgpt-shell-send-and-review-region)
(keymap-global-set "C-c l SPC" #'chatgpt-shell-send-region)
(keymap-global-set "C-c l c" #'chatgpt-shell-prompt-compose)
(keymap-global-set "C-c l i" #'chatgpt-shell-quick-insert)
(keymap-global-set "C-c l l" #'chatgpt-shell)
(keymap-global-set "C-c l m" #'chatgpt-shell-inline)
(keymap-global-set "C-c l o" #'chatgpt-shell-other-buffer)
(keymap-global-set "C-c l p" #'chatgpt-shell-proofread-region)
(keymap-global-set "C-c l r" #'chatgpt-shell-refactor-code)
(keymap-global-set "C-c l s" #'chatgpt-shell-rephrase-sentences)
(keymap-global-set "C-c l u" #'chatgpt-shell-generate-unit-test)
(keymap-global-set "C-c l x" #'chatgpt-shell-explain-code)
(keymap-set chatgpt-shell-mode-map "C-c C-S-p" #'chatgpt-shell-load-awesome-prompts)
(keymap-set chatgpt-shell-mode-map "C-c C-S-s" #'chatgpt-shell-show-prompt)


;;; chronos

(require 'chronos)

(defun chronos-osascript-notify (c)
  "(For Mac OS) Notify expiration of timer C using osascript."
  (let ((shell-command-string (concat "osascript -e "
                                      "'"
                                      "display notification "
                                      "\"" (chronos--time-string c) ": " (chronos--message c) "\""
                                      "with title "
                                      "\"Emacs\""
                                      "'")))
    (shell-command shell-command-string)))

(defun chronos-alert (c)
  "(For Mac OS) Notify expiration of timer C using alert."
  (alert (chronos--message c)
         :title (concat (chronos--time-string c) ": Timer expired")
         :style 'osx-notifier))

(setq chronos-standard-timers '("0:0:30/30-second finished"
                                "5/5-minute timer finished"
                                "15/15-minute timer finished"
                                "30/30-minutes timer finished"
                                "01:00/1-hour timer finished")
      chronos-expiry-functions '(chronos-message-notify
                                 ;; For macOS: Use chronos-alert instead of chronos-desktop-notifications-notify
                                 chronos-desktop-notifications-notify))

(defun chronos-load ()
  "Load chronos."
  (interactive)
  (if (get-buffer chronos-buffer-name)
      (switch-to-buffer chronos-buffer-name)
    (chronos-initialize)))

(keymap-global-set "C-c T" #'chronos-load)


;;; command-log-mode

;; Don't bind automatically after loading module
(setq command-log-mode-key-binding-open-log nil)

(require 'command-log-mode)
(setq command-log-mode-window-font-size 0
      command-log-mode-window-size 60
      command-log-mode-is-global t
      command-log-mode-open-log-turns-on-mode t)

;; The original function definition hard-coded the text scale,
;; ignoring `command-log-mode-window-font-size'.
(defun clm/open-command-log-buffer (&optional arg)
  "Open (and create, if non-existant) a buffer used for logging keyboard commands.
If ARG is Non-nil, the existing command log buffer is cleared."
  (interactive "P")
  (with-current-buffer
      (setq clm/command-log-buffer
            (get-buffer-create " *command-log*"))
    (text-scale-set command-log-mode-window-font-size))
  (when arg
    (with-current-buffer clm/command-log-buffer
      (erase-buffer)))
  (let ((new-win (split-window-horizontally
                  (- 0 command-log-mode-window-size))))
    (set-window-buffer new-win clm/command-log-buffer)
    (set-window-dedicated-p new-win t)))

(keymap-global-set "C-c c l" #'clm/toggle-command-log-buffer)


;;; company

(skipped
 (require 'company)
 (setq company-lighter-base "Comp"
       company-minimum-prefix-length 2
       company-idle-delay 0.3
       company-show-quick-access 'left
       company-selection-wrap-around 1
       company-require-match nil
       company-dabbrev-minimum-length 3
       company-dabbrev-downcase nil
       company-transformers '(company-sort-by-occurrence
                              company-sort-by-backend-importance)
       company-global-modes '(not eshell-mode term-mode))
 (add-to-list 'company-backends 'company-native-complete)
 (add-to-list 'company-backends 'company-go)
 (add-to-list 'company-backends 'company-restclient)
 ;; This backend sometimes produces an error when in other modes, so
 ;; enable only on nix-mode
 (with-eval-after-load 'nix-mode
   (add-hook 'nix-mode-hook (lambda ()
                              (interactive)
                              (make-local-variable 'company-backends)
                              (add-to-list 'company-backends 'company-nixos-options))))
 (keymap-set company-active-map "C-n" #'company-select-next)
 (keymap-set company-active-map "C-p" #'company-select-previous)
 (keymap-set company-active-map "C-c C-/" #'company-other-backend)
 (add-hook 'after-init-hook #'global-company-mode))


;;; conf-mode

(add-to-list 'auto-mode-alist '("\\.env.*\\'" . conf-mode))


;;; consult

(require 'consult)
(setq consult-preview-key "C-SPC"
      consult-narrow-key "<")
(keymap-global-set "<remap> <repeat-complex-command>" #'consult-complex-command)
(keymap-global-set "<remap> <switch-to-buffer-other-frame>" #'consult-buffer-other-frame)
(keymap-global-set "<remap> <switch-to-buffer-other-window>" #'consult-buffer-other-window)
(keymap-global-set "<remap> <switch-to-buffer>" #'consult-buffer)
(keymap-global-set "<remap> <yank-pop>" #'consult-yank-pop)
(keymap-global-set "C-c m M-x" #'consult-mode-command)
(keymap-global-set "C-c o j" #'consult-org-agenda)
(keymap-set goto-map "I" #'consult-imenu-multi)
(keymap-set goto-map "M" #'consult-global-mark)
(keymap-set goto-map "d" #'consult-dir)
(keymap-set goto-map "h" #'consult-history)
(keymap-set goto-map "i" #'consult-imenu)
(keymap-set goto-map "m" #'consult-mark)
(keymap-set minibuffer-local-map "M-g d" #'consult-dir)
(keymap-set minibuffer-local-map "M-g f" #'consult-dir-jump-file)
(keymap-set minibuffer-local-map "M-s" #'consult-history)


;;; consult-org-roam

(with-eval-after-load 'org-roam
  (require 'consult-org-roam)
  (setq consult-org-roam-grep-func #'consult-ripgrep
        consult-org-roam-buffer-after-buffers nil)
  (consult-org-roam-mode 1)
  (keymap-global-set "C-c n f" #'consult-org-roam-file-find)
  (keymap-global-set "C-c n g" #'consult-org-roam-search)
  (keymap-set org-mode-map "C-c n <" #'consult-org-roam-backlinks)
  (keymap-set org-mode-map "C-c n >" #'consult-org-roam-forward-links))


;;; corfu

(require 'corfu)
(setq corfu-auto nil
      corfu-auto-delay 0.2
      corfu-auto-prefix 3)
(keymap-set corfu-map "C-s" #'corfu-next)
(keymap-set corfu-map "C-." #'corfu-next)
(keymap-set corfu-map "C-r" #'corfu-previous)
(keymap-set corfu-map "C-," #'corfu-previous)
(global-corfu-mode)


;;; corfu-echo

(require 'corfu-echo)
(setq corfu-echo-delay '(1.0 . 0.5))
(corfu-echo-mode)


;;; cov

(require 'cov)
(setq cov-coverage-mode t)

(defun my-python-run-coverage ()
  "Interpret the .coverage file in the project root and enable display code coverage."
  (interactive)
  (if current-prefix-arg
      (progn
        (let ((cmd (format "cd %s; coverage json" (project-root (project-current)))))
          (call-process-shell-command cmd))
        (cov-mode 1))
    (call-interactively #'cov-mode)))

(with-eval-after-load 'python
  (keymap-set python-mode-map "C-c C C" #'my-python-run-coverage))


;;; devdocs

(keymap-global-set "C-h D" #'devdocs-lookup)


;;; diff-hl

(require 'diff-hl)
(let ((map diff-hl-mode-map))
  (keymap-set map "C-c d h a" #'diff-hl-amend-mode)
  (keymap-set map "C-c d h f" #'diff-hl-flydiff-mode))
(keymap-global-set "C-c d h h" #'global-diff-hl-mode)
(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
(global-diff-hl-mode 1)


;;; diminish
(require 'diminish)
(defun diminish-mode (module mode &optional to-what)
  "Diminish MODE after loading MODULE to TO-WHAT."
  (with-eval-after-load module (diminish mode to-what)))

(defun my-diminish-config ()
  "Load diminished modes."
  (diminish-mode 'action-lock 'action-lock-mode)
  (diminish-mode 'autorevert 'auto-revert-mode)
  (diminish-mode 'consult-org-roam 'consult-org-roam-mode)
  (diminish-mode 'org-indent 'org-indent-mode)
  (diminish-mode 'orgalist 'orgalist-mode)
  (diminish-mode 'prettier-js 'prettier-js-mode)
  (diminish-mode 'selected 'selected-minor-mode)
  (diminish-mode 'smartparens 'smartparens-mode)
  (diminish-mode 'subword 'subword-mode)
  (diminish-mode 'yasnippet 'yas-minor-mode))
(add-hook 'after-init-hook #'my-diminish-config)


;;; dired

(require 'dired)
(setq dired-auto-revert-buffer t
      dired-dwim-target t
      dired-hide-details-hide-information-lines nil
      dired-hide-details-hide-symlink-targets nil
      dired-isearch-filenames t
      dired-kill-when-opening-new-dired-buffer nil
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      my-dired-listing-switches "--group-directories-first -lhv"
      my-dired-listing-a-switch "")

(when (eq system-type 'darwin)
  ;; Don't forget `brew install coreutils' in MacOS
  (setq insert-directory-program "gls"))

(defun dired-set-listing-switches ()
  "Update `dired-listing-switches'."
  (setq dired-listing-switches (concat my-dired-listing-switches
                                       my-dired-listing-a-switch)))
(dired-set-listing-switches)

(defun dired-toggle-other-files-visibility ()
  (interactive)
  (setq my-dired-listing-a-switch (if (string-empty-p my-dired-listing-a-switch) " -a" ""))
  (message "%s other files" (if (string-empty-p my-dired-listing-a-switch) "Hide" "Show"))
  (dired-sort-other (dired-set-listing-switches)))

(defun dired-ediff-marked-files ()
  "Run `ediff-files' on 2 marked files.
Inspired by https://oremacs.com/2017/03/18/dired-ediff"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if (not (= (length files) 2))
        (error "Marked files != 2")
      (ediff-files (cl-first files) (cl-second files)))))

(defun dired-apply-to-marked-files (function)
  "Prompt for FUNCTION and pass the marked Dired entries as its arguments."
  (interactive "CApply to marked dired entries: ")
  (if (not (fboundp function))
      (error "%s is not a function" function)
    (let* ((arity (func-arity function))
           (min-arity (car arity))
           (max-arity (cdr arity))
           (input-length (length (dired-get-marked-files))))
      (if (not (or (and (numberp max-arity)
                        (>= input-length min-arity)
                        (<= input-length max-arity))
                   (and (>= input-length min-arity)
                        (eq 'many max-arity))))
          (error "Input does not match function arity")
        (apply function (dired-get-marked-files))))))

(keymap-set dired-mode-map "z" #'dired-up-directory)
(keymap-set dired-mode-map "C-." #'dired-toggle-other-files-visibility)
(keymap-set dired-mode-map "C-c m e" #'dired-create-empty-file)
(keymap-set dired-mode-map "C-c m d" #'dired-ediff-marked-files)
(keymap-set dired-mode-map "C-c m !" #'dired-apply-to-marked-files)
(with-eval-after-load 'dired-subtree
  (keymap-set dired-mode-map "<tab>" #'dired-subtree-toggle)
  (keymap-set dired-mode-map "<backtab>" #'dired-subtree-cycle))
(with-eval-after-load 'org
  (keymap-set dired-mode-map "C-c C-a" #'org-attach-dired-to-subtree))

(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(dired-recent-mode 1)


;;; dired-ediff-a-b

(defvar dired-ediff-file-a nil "File A to compare in ediff using `dired-ediff-a-b'.")
(defvar dired-ediff-file-b nil "File B to compare in ediff using `dired-ediff-a-b'.")

(defun dired-ediff-a-b-cleanup ()
  "Cleanup after doing `dired-ediff-a-b'."
  (setq dired-ediff-file-a nil
        dired-ediff-file-b nil)
  (remove-hook 'ediff-quit-hook #'dired-ediff-a-b-cleanup))

(defun dired-ediff-a-b ()
  "Compare 2 files using ediff from any Dired buffer.
The first time this is called the selected file in Dired is
treated as file A, and the file selected during second call will
be file B."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (or current-prefix-arg (not dired-ediff-file-a))
        (progn
          (setq dired-ediff-file-a file)
          (message "dired-ediff-a-b: File A set"))
      (setq dired-ediff-file-b file)
      (add-hook 'ediff-quit-hook #'dired-ediff-a-b-cleanup)
      (ediff-files dired-ediff-file-a dired-ediff-file-b))))

(keymap-set dired-mode-map "C-c d d" #'dired-ediff-a-b)


;;; dired-marked

(require 'dired-marked)
(keymap-set dired-mode-map "* C" #'dired-marked-copy-file-to-marked-directories)
(keymap-set dired-mode-map "* U" #'dired-marked-unmark-all)


;;; dired-narrow (from dired-hacks)

(require 'dired-narrow)
(setq dired-narrow-map nil)
(keymap-set dired-mode-map "/ /" #'dired-narrow)
(keymap-set dired-mode-map "/ r" #'dired-narrow-regexp)


;;; dired-sidebar

(require 'dired-sidebar)
(setq dired-sidebar-should-follow-file nil
      dired-sidebar-theme 'none)
(add-hook 'dired-sidebar-mode-hook (lambda () (interactive) (hl-line-mode -1)))

(keymap-set dired-sidebar-mode-map "z" #'dired-sidebar-up-directory)
(keymap-global-set "C-c d s" #'dired-sidebar-toggle-sidebar)
(keymap-global-set "C-c d S" #'dired-sidebar-jump-to-sidebar)


;;; dired-subtree

(require 'dired-subtree)
(setq dired-subtree-use-backgrounds nil)


;;; easy-kill

(keymap-global-set "<remap> <kill-ring-save>" #'easy-kill)
(keymap-global-set "<remap> <mark-word>" #'easy-mark)


;;; eat

(require 'eat)
(setq eshell-visual-commands nil)
(eat-eshell-mode)


;;; ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)
(defvar ediff-temp-buffer-a "*diff-a*")
(defvar ediff-temp-buffer-b "*diff-b*")

(defun ediff-restore-window-config ()
  "Restore the saved window configuration in `ediff-previous-window-config'."
  (set-window-configuration ediff-previous-window-config)
  (kill-buffer ediff-temp-buffer-a)
  (kill-buffer ediff-temp-buffer-b))

(defun ediff-save-windows-config ()
  "Save current window configuration in `ediff-previous-window-config'."
  (setq ediff-previous-window-config (current-window-configuration)))

(defun ediff-last-2-kills ()
  "Run ediff on the last 2 kills."
  (interactive)
  (let ((text-a (current-kill 1 t))
        (text-b (current-kill 0 t))
        (a (generate-new-buffer (generate-new-buffer-name ediff-temp-buffer-a)))
        (b (generate-new-buffer (generate-new-buffer-name ediff-temp-buffer-b))))
    (with-current-buffer a (insert text-a))
    (with-current-buffer b (insert text-b))
    (ediff-buffers a b)))

(keymap-global-set "C-c d b" #'ediff-buffers)
(keymap-global-set "C-c d c" #'ediff-current-file)
(keymap-global-set "C-c d d" #'ediff-files)
(keymap-global-set "C-c d k" #'ediff-last-2-kills)
(keymap-global-set "C-c d r l" #'ediff-regions-linewise)
(keymap-global-set "C-c d r w" #'ediff-regions-wordwise)
(add-hook 'ediff-before-setup-hook #'ediff-save-windows-config)
(add-hook 'ediff-quit-hook #'ediff-restore-window-config)


;;; eglot

(require 'eglot)
(setq eglot-autoshutdown t
      flymake-no-changes-timeout 1)
(setq-mode-local python-mode
                 eglot-ignored-server-capabilities '(:documentHighlightProvider
                                                     :hoverProvider))
(setq-mode-local nix-mode
                 eglot-ignored-server-capabilities '(:documentHighlightProvider
                                                     :hoverProvider
                                                     :documentRangeFormattingProvider
                                                     :inlayHintProvider))
(setq-mode-local typescript-ts-mode
                 eglot-ignored-server-capabilities '(:documentHighlightProvider
                                                     :hoverProvider
                                                     :inlayHintProvider))


;;; electric

(setq electric-layout-rules '((?\{ . around)
                              (?\} . around)))


;;; emms

;; playback
(require 'emms-setup)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-player-simple)
(require 'emms-player-mpv)
(require 'emms-cache)
;; track information
(require 'emms-cue)
(require 'emms-info-exiftool)
(require 'emms-info-native)
(require 'emms-show-all)
(require 'emms-mode-line)
;; browsing
(require 'emms-browser)
(require 'emms-playlist-mode)
(require 'emms-playlist-limit)
(require 'emms-metaplaylist-mode)
(require 'dired)

(defvar my-emms-playlist-directory "~/Music/playlists/")
(defvar my-emms-library-directory "~/Music/library/")
(defvar my-emms-flac-directory "~/Music/flac/")
(defvar my-emms-loop-timer nil)

;; track filters
(emms-browser-make-filter "all" 'ignore)
(emms-browser-make-filter "flac" (emms-browser-filter-only-dir my-emms-flac-directory))
(emms-browser-make-filter "library" (emms-browser-filter-only-dir my-emms-library-directory))
(emms-browser-set-filter (assoc "library" emms-browser-filters))

(defun emms-info-my-track-description (track)
  "Return a description of TRACK."
  (let ((artist (or
                 (emms-track-get track 'info-albumartist)
                 (emms-track-get track 'info-artist)))
        (title  (emms-track-get track 'info-title))
        (duration (emms-track-get track 'info-playing-time))
        (discnumber (emms-track-get track 'info-discnumber))
        (tracknumber (emms-track-get track 'info-tracknumber)))
    (if (or artist title)
        (concat
         (when (stringp tracknumber) (format "%2s. " tracknumber)) (when (numberp tracknumber) (format "%2d. " tracknumber))
         (when (stringp discnumber) (format "(%s) " discnumber)) (when (numberp discnumber) (format "(%d) " discnumber))
         (when artist (format "%s - " artist))
         title
         (format "   (%02d:%02d)" (/ duration 60) (% duration 60)))
      (emms-track-simple-description track))))

(defun optional-flag-string (flag string)
  (if flag string ""))

(defun emms-my-mode-line-display ()
  (concat " ["
          (buffer-name emms-playlist-buffer)
          (if emms-player-paused-p " paused")
          (optional-flag-string (with-current-emms-playlist emms-random-playlist) " random")
          (optional-flag-string (with-current-emms-playlist emms-repeat-track) " repeat-track")
          " ] "))

(setq emms-player-list '(emms-player-mpv
                         emms-player-alsaplayer
                         emms-player-mpg321
                         emms-player-ogg123)
      emms-info-functions '(emms-info-native emms-info-cueinfo)
      emms-repeat-playlist t
      emms-info-asynchronously nil
      emms-playlist-mode-open-playlists t
      emms-source-playlist-default-format 'm3u
      emms-track-description-function #'emms-info-my-track-description
      emms-mode-line-mode-line-function #'emms-my-mode-line-display
      emms-seek-seconds 5)

(add-to-list 'emms-track-initialize-functions #'emms-info-initialize-track)
(add-hook 'emms-player-paused-hook #'emms-mode-line-alter)
(add-hook 'emms-browser-mode-hook #'hl-line-mode)
(add-hook 'emms-playlist-mode-hook #'hl-line-mode)

(emms-cache 1)
(emms-mode-line-mode)

(defun emms-metaplaylist-mode-new-buffer-no-update (buffer-name)
  "Creates a new buffer playlist buffer BUFFER-NAME but skip `emms-metaplaylist-mode-update'."
  (interactive "sBuffer Name: ")
  (if (get-buffer buffer-name)
      (error "Buffer must not exist.")
    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        (emms-playlist-mode)
        (setq emms-playlist-buffer-p t)))))

(defun emms-browser-replace-playlist ()
  (interactive)
  (emms-browser-clear-playlist)
  (emms-browser-add-tracks))

(defun emms-my-play-playlist ()
  (interactive)
  (let ((emms-source-file-default-directory my-emms-playlist-directory))
    (emms-playlist-set-playlist-buffer emms-playlist-buffer-name)
    (call-interactively #'emms-play-playlist)))

(defun emms-my-metaplaylist-play-playlist (file)
  (interactive (list (read-file-name "Play playlist file: "
                                     my-emms-playlist-directory
                                     my-emms-playlist-directory
                                     t)))
  (let* ((filename (file-name-nondirectory file))
         (playlist-name (concat " *EMMS Playlist: " filename "*")))
    (unless (get-buffer playlist-name)
      (emms-metaplaylist-mode-new-buffer-no-update playlist-name))
    (emms-playlist-set-playlist-buffer playlist-name)
    (emms-play-playlist file)))

(defun emms-my-metaplaylist-find-playlist (file)
  (interactive (list (read-file-name "Find playlist file: "
                                     my-emms-playlist-directory
                                     my-emms-playlist-directory
                                     t)))
  (let* ((filename (file-name-nondirectory file))
         (playlist-name (concat " *EMMS Playlist: " filename "*")))
    (unless (get-buffer playlist-name)
      (emms-metaplaylist-mode-new-buffer playlist-name)
      (with-current-buffer playlist-name
        (emms-insert-playlist file)))))

(defun emms-my-metaplaylist-find-all-playlists ()
  (interactive)
  (let ((directory (if current-prefix-arg
                       (read-directory-name "Find all playlists in: "
                                            my-emms-playlist-directory
                                            my-emms-playlist-directory
                                            t)
                     my-emms-playlist-directory)))
    (message "Finding all playlist files at %s" directory)
    (dolist (file (directory-files directory t directory-files-no-dot-files-regexp))
      (emms-my-metaplaylist-find-playlist file)))
  (emms-metaplaylist-mode-update))

(defun emms-my-metaplaylist-mode-go ()
  (interactive)
  (when (not (emms-playlist-buffer-list))
    ;; Create an empty playlist buffer if none exist yet
    (emms-playlist-current-clear)
    (emms-my-metaplaylist-find-all-playlists))
  (emms-metaplaylist-mode-go))

(defun emms-my-playlist-save ()
  (interactive)
  (let ((emms-source-file-default-directory my-emms-playlist-directory))
    (call-interactively #'emms-playlist-save)))

(defun emms-playlist-write (format file)
  "Store the current playlist to FILE as the type FORMAT.
The default format is specified by `emms-source-playlist-default-format'."
  (interactive (list (emms-source-playlist-read-format)
                     (read-file-name "Store as: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     nil)))
  (if emms-playlist-buffer-p
      (let ((emms-playlist-buffer-to-write (current-buffer)))
        (with-temp-buffer
          (emms-source-playlist-unparse format
                                        emms-playlist-buffer-to-write
                                        (current-buffer))
          (let ((backup-inhibited t))
            (write-file file emms-source-playlist-ask-before-overwrite))))
    (message "aborting save")))

(defun emms-my-playlist-write ()
  (interactive)
  (let ((emms-source-file-default-directory my-emms-playlist-directory))
    (call-interactively #'emms-playlist-write)))

(defun emms-my-toggle-random-playlist ()
  (interactive)
  (emms-toggle-random-playlist)
  (setq emms-repeat-track nil)
  (emms-mode-line-alter))

(defun emms-my-toggle-repeat-track ()
  (interactive)
  (emms-toggle-repeat-track)
  (when emms-repeat-track
    (customize-set-variable 'emms-random-playlist nil))
  (emms-mode-line-alter))

(defun emms-my-stop-loop ()
  "Stop the currently running loop."
  (interactive)
  (when (timerp my-emms-loop-timer)
    (message "Stopping EMMS loop")
    (cancel-timer my-emms-loop-timer)))

(defun emms-my-toggle-loop (arg)
  "Continuously loop a the last ARG seconds of the currently playing track.
Use a negative argument to stop the loop."
  (interactive "p")
  (emms-my-stop-loop)
  (when (>= arg 1)
    (let ((length (if (= 1 arg) 5 arg)))
      (message "Starting EMMS loop of %s seconds" length)
      (setq my-emms-loop-timer (run-with-timer 0 length (lambda () (emms-seek (- length))))))))

(defun emms-playlist-my-add-track-to-playlist (buffer)
  (interactive
   (list (let* ((buf-list (mapcar #'(lambda (buf)
                                      (list (buffer-name buf)))
                                  (emms-playlist-buffer-list)))
                (sorted-buf-list (sort buf-list
                                       #'(lambda (lbuf rbuf)
                                           (< (length (car lbuf))
                                              (length (car rbuf)))))))
           (emms-completing-read "Playlist buffer to add track: "
                                 sorted-buf-list nil t))))
  (let ((previous-buffer emms-playlist-buffer)
        (previous-selection (overlay-start emms-playlist-mode-selected-overlay)))
    (emms-playlist-ensure-playlist-buffer)
    (emms-playlist-set-playlist-buffer buffer)
    (emms-playlist-mode-add-contents)
    (emms-playlist-set-playlist-buffer previous-buffer)
    (emms-playlist-select previous-selection)))

(defun emms-my-metaplaylist-view-playlist ()
  "Open the EMMS playlist without changing the current active playlist."
  (interactive)
  (let ((buffer (get-buffer
                 (buffer-substring (line-beginning-position)
                                   (line-end-position)))))
    (switch-to-buffer buffer)))

(defvar emms-my-insert-track-to-playlist-destination emms-playlist-buffer-name
  "What playlist to use in `emms-my-insert-track-to-playlist'.")

(defun emms-my-insert-track-to-playlist ()
  "Add the current track at point to the playlist set in `emms-my-insert-track-to-playlist-destination'.
When a prefix is used, ask where to insert the track and save it to `emms-my-insert-track-to-playlist-destination'."
  (interactive)
  (let* ((name (emms-track-get (emms-playlist-track-at) 'name))
         (dest-playlist (if (not current-prefix-arg)
                            emms-my-insert-track-to-playlist-destination
                          (completing-read
                           "Insert track in: "
                           (mapcar #'buffer-name emms-playlist-buffers) nil t))))

    (unless (eq emms-my-insert-track-to-playlist-destination dest-playlist)
      (setq emms-my-insert-track-to-playlist-destination dest-playlist))
    (save-excursion
      (with-current-buffer dest-playlist
        (emms-insert-file name)))
    (message "Added to: %s" dest-playlist)))

(keymap-global-set "<remap> <emms-playlist-save>" #'emms-my-playlist-save)
(keymap-global-set "C-c M %" #'emms-my-toggle-random-playlist)
(keymap-global-set "C-c M 1" #'emms-my-toggle-repeat-track)
(keymap-global-set "C-c M B" #'emms-smart-browse)
(keymap-global-set "C-c M L" #'emms-my-metaplaylist-play-playlist)
(keymap-global-set "C-c M P" #'emms-pause)
(keymap-global-set "C-c M b" #'emms-my-metaplaylist-mode-go)
(keymap-global-set "C-c M i" #'emms-show-all)
(keymap-global-set "C-c M l" #'emms-playlist-mode-switch-buffer)
(keymap-global-set "C-c M m" #'emms-mode-line-mode)
(keymap-global-set "C-c M n" #'emms-next)
(keymap-global-set "C-c M p" #'emms-previous)
(keymap-global-set "C-c M s" #'emms-stop)
(keymap-set dired-mode-map "C-c M A" #'emms-play-dired)
(keymap-set dired-mode-map "C-c M a" #'emms-add-dired)
(keymap-set emms-browser-mode-map "R" #'emms-browser-replace-playlist)
(keymap-set emms-browser-mode-map "a" #'emms-add-directory-tree)
(keymap-set emms-mark-mode-map "M" #'emms-mark-mode-disable)
(keymap-set emms-metaplaylist-mode-map "G" #'emms-my-metaplaylist-find-all-playlists)
(keymap-set emms-metaplaylist-mode-map "f" #'emms-my-metaplaylist-find-playlist)
(keymap-set emms-metaplaylist-mode-map "v" #'emms-my-metaplaylist-view-playlist)
(keymap-set emms-metaplaylist-mode-map "q" #'kill-current-buffer)
(keymap-set emms-metaplaylist-mode-map "z" #'emms-playlist-mode-go)
(keymap-set emms-playlist-mode-map "%" #'emms-shuffle)
(keymap-set emms-playlist-mode-map "&" #'emms-my-toggle-loop)
(keymap-set emms-playlist-mode-map "," #'emms-seek-backward)
(keymap-set emms-playlist-mode-map "." #'emms-seek-forward)
(keymap-set emms-playlist-mode-map "<left>" #'emms-seek-backward)
(keymap-set emms-playlist-mode-map "<right>" #'emms-seek-forward)
(keymap-set emms-playlist-mode-map "A" #'emms-playlist-my-add-track-to-playlist)
(keymap-set emms-playlist-mode-map "C-x C-w" #'emms-my-playlist-write)
(keymap-set emms-playlist-mode-map "F" #'emms-show-all)
(keymap-set emms-playlist-mode-map "M" #'emms-mark-mode)
(keymap-set emms-playlist-mode-map "i" #'emms-my-insert-track-to-playlist)
(keymap-set emms-playlist-mode-map "z" #'emms-metaplaylist-mode-go)


;;; epa

(require 'epa)
(setq epa-keys-select-method 'minibuffer)


;;; epg

(setq epg-pinentry-mode 'loopback)


;;; erc

(require 'erc)
(erc-dcc-mode 1)


;;; esh-help

(with-eval-after-load 'eshell
  (require 'esh-help)
  (setup-esh-help-eldoc))


;;; esh-toggle

(with-eval-after-load 'eshell
  (require 'esh-toggle)
  (keymap-global-set "C-c e t" #'eshell-toggle))


;;; eshell

(require 'eshell)
(setq eshell-hist-ignoredups t
      eshell-history-size 10000
      eshell-ls-dired-initial-args '("-h")
      eshell-ls-initial-args '("-h")
      ;; eat-eshell will handle these
      ;; eshell-visual-subcommands '(("git" "log" "diff" "show" "shortlog")
      ;;                             ("aws" "cloudfront" "dynamodb"))
      my-eshell-names-alist '(("nix-config" . "~/prj/nix-config")
                              ("bash-scripts" . "~/prj/bash-scripts")
                              ("other")))
(add-to-list 'eshell-modules-list 'eshell-rebind)
(add-to-list 'eshell-modules-list 'eshell-xtra)

(defun eshell-ask (eshell-name)
  (interactive (list (completing-read "Eshell: " (mapcar #'car my-eshell-names-alist))))
  (let* ((eshell-buffer-name (concat "*eshell-" eshell-name "*"))
         (eshell-buffer (get-buffer eshell-buffer-name))
         (path-to-cd (cdr (assoc eshell-name my-eshell-names-alist))))
    (eshell)
    (unless eshell-buffer
      (when path-to-cd
        (insert (concat "cd " path-to-cd))
        (eshell-send-input)))))

(defun eshell-new ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'eshell)))

(defun eshell-other ()
  (interactive)
  (let ((eshell-buffer-name "*eshell-other*"))
    (eshell)))

(defun my-eshell-config ()
  (setq-local completion-auto-help t)
  (setenv "PAGER" "cat")
  (keymap-set eshell-mode-map "C-c C-<backspace>" (lambda ()
                                                    (interactive)
                                                    (eshell/clear-scrollback))))
(add-hook 'eshell-mode-hook #'my-eshell-config)
(keymap-global-set "C-c e e" #'eshell)
(keymap-global-set "C-c e E" #'eshell-other)
(keymap-global-set "C-c e ?" #'eshell-ask)
(keymap-global-set "C-c e +" #'eshell-new)


;;; eshell-up

(with-eval-after-load 'eshell
  (require 'eshell-up))


;;; eshell-z

(with-eval-after-load 'eshell
  (require 'eshell-z))


;;; expand-region

(require 'expand-region)
(keymap-global-set "C-c e r" #'er/expand-region)
(keymap-global-set "C-c e [" #'er/mark-inside-pairs)
(keymap-global-set "C-c e {" #'er/mark-outside-pairs)
(keymap-global-set "C-c e '" #'er/mark-inside-quotes)
(keymap-global-set "C-c e \"" #'er/mark-outside-quotes)


;;; ffap

(keymap-global-set "C-c f ." #'ffap)


;;; find-dired

(require 'find-dired)
(defun find-name-dired-current (pattern)
  "Call `find-name-dired' in the current directory to search for files matching PATTERN."
  (interactive "sFind-name (filename wildcard): ")
  (let* ((case-fold-search nil)
         (find-name-arg
          (if (or current-prefix-arg
                  (string-match-p "[[:upper:]]" pattern))
              "-name"
            "-iname")))
    (find-name-dired (dired-current-directory) pattern)))

(defun find-grep-dired-current (regexp)
  "Call `find-grep-dired' in the current directory to match REGEXP in files."
  (interactive "sFind-grep (grep regexp): ")
  (let* ((case-fold-search nil)
         (grep-ignore-case-flag
          (if (or current-prefix-arg
                  (string-match-p "[[:upper:]]" regexp))
              ""
            " -i"))
         (find-grep-options (concat find-grep-options grep-ignore-case-flag)))
    (find-grep-dired (dired-current-directory) regexp)))

(keymap-set dired-mode-map "C-c m f" #'find-name-dired-current)
(keymap-set dired-mode-map "C-c m g g" #'find-grep-dired-current)


;;; flycheck-kotlin

(defun flycheck-kotlin-config ()
  (require 'flycheck-kotlin)
  (flycheck-kotlin-setup))
(eval-after-load 'flycheck #'flycheck-kotlin-config)


;;; gnus

(require 'gnus)
(setq gnus-select-method '(nnnil "")
      gnus-secondary-select-methods
      '((nnimap "proton"
                (nnimap-address "127.0.0.1")
                (nnimap-server-port "imap")
                (nnimap-stream plain)
                (nnimap-user "nicolaisingh@pm.me")))
      gnus-parameters
      '(("INBOX"
         (total-expire . t)
         (expiry-target . "nnimap+proton:Archive")
         (expiry-wait . 84))

        ("Spam"
         (total-expire . t)
         (expiry-target . delete)
         (expiry-wait . 56))

        ("daily.summary"
         (gnus-summary-line-format "%U%R%z%(%-20,20f%) : %s %1{%[ %&user-date; %]%}\n")
         (gnus-show-threads nil)
         (gnus-use-scoring nil)
         (gnus-article-sort-functions '(gnus-thread-sort-by-most-recent-date)))

        ("notifications"
         (gnus-summary-line-format "%U%R%z%(%-20,20f%) : %s %1{%[ %&user-date; %]%}\n")
         (gnus-show-threads nil)
         (gnus-use-scoring nil)
         (gnus-article-sort-functions '(gnus-thread-sort-by-most-recent-date)))

        ("notifications$"
         (total-expire . t)
         (expiry-target . delete)
         (expiry-wait . 56))

        ("lists"
         (total-expire . t)
         (expiry-target . delete)
         (expiry-wait . 56)))

      gnus-expert-user nil
      gnus-inhibit-startup-message t
      gnus-interactive-exit 'quiet
      gnus-large-newsgroup 1000
      gnus-novice-user nil
      gnus-summary-line-format "%U%R%z%(%-20,20f%) %2t : %B %1{%[%&user-date;%]%}\n"
      gnus-use-cache t
      gnus-use-trees nil
      ;; FIXME
      mm-text-html-renderer 'gnus-w3m
      message-signature "Drew"
      gnus-max-image-proportion 0.5)


;;; gnus-async

(require 'gnus-async)
(setq gnus-asynchronous t)


;;; gnus-cache

(require 'gnus-cache)
(setq gnus-cacheable-groups nil
      gnus-uncacheable-groups "^nnml\\|^nnfolder\\|^nnmaildir")


;;; gnus-dired
(require 'gnus-dired)
(add-hook 'dired-mode-hook #'gnus-dired-mode)


;;; gnus-group

(require 'gnus-group)
(setq gnus-keep-same-level t
      gnus-group-goto-unread t
      gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)\n"
      gnus-group-sort-function '(gnus-group-sort-by-alphabet gnus-group-sort-by-level)
      gnus-permanently-visible-groups "INBOX")
(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)

(defun my-gnus-offlineimap-sync ()
  (interactive)
  (async-shell-command "offlineimap -u ttyui" (get-buffer-create "*offlineimap*")))

(defun my-gnus-mbsync ()
  (interactive)
  (async-shell-command "systemctl start mbsync && journalctl -u mbsync -f" (get-buffer-create "*mbsync*")))

(keymap-set gnus-group-mode-map "v s" #'my-gnus-mbsync)


;;; gnus-salt

(require 'gnus-salt)
(setq gnus-generate-tree-function #'gnus-generate-horizontal-tree)


;;; gnus-score

(require 'gnus-score)
(setq gnus-default-adaptive-score-alist
      '((gnus-saved-mark (subject 15))
        (gnus-replied-mark (subject 10) (from 5))
        (gnus-read-mark (subject 5) (from 1))
        (gnus-del-mark (subject -5) (from -1))
        (gnus-killed-mark (subject -10) (from -5))
        (gnus-catchup-mark (subject -15)))
      gnus-score-expiry-days 90
      gnus-use-adaptive-scoring '(line)
      gnus-adaptive-pretty-print t
      gnus-summary-mark-below -50)


;;; gnus-search

(require 'gnus-search)
(setq gnus-search-use-parsed-queries nil)


;;; gnus-start

(require 'gnus-start)
(setq gnus-always-read-dribble-file t
      gnus-subscribe-newsgroup-method 'gnus-subscribe-topics
      ;; 1-5 : subscribed
      ;; 6-7 : unsubscribed
      ;; 8   : zombie
      ;; 9   : killed
      gnus-activate-level 2
      gnus-level-default-subscribed 3

      ;; Don't maintain the .newsrc file
      gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil)


;;; gnus-sum

(require 'gnus-sum)
(setq gnus-auto-select-next nil
      gnus-sort-gathered-threads-function #'gnus-thread-sort-by-date
      gnus-sum-thread-tree-false-root nil
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├── "
      gnus-sum-thread-tree-root nil
      gnus-sum-thread-tree-single-indent nil
      gnus-sum-thread-tree-single-indent nil
      gnus-sum-thread-tree-single-leaf "└── "
      gnus-sum-thread-tree-vertical "│ "
      gnus-summary-display-arrow t
      gnus-summary-next-group-on-exit nil
      gnus-summary-same-subject ""
      gnus-thread-hide-subtree t
      gnus-thread-sort-functions '((not gnus-thread-sort-by-number)))

(defun my-gnus-trash-article (arg)
  (interactive "P")
  (cond
   ((string-match "nnimap\\+proton" gnus-newsgroup-name)
    (gnus-summary-move-article arg "nnimap+proton:Trash"))
   (t
    (message "no trash"))))

(keymap-set gnus-summary-mode-map "v <backspace>" #'my-gnus-trash-article)


;;; gnus-topic

(require 'gnus-topic)
(setq gnus-topic-display-empty-topics t
      gnus-topic-line-format "%i[ %(%{%A: %n%}%) ]%v\n")


;;; gnus-util

(require 'gnus-util)
(setq gnus-verbose 10)


;;; go-mode

(require 'go-mode)


;;; graphviz-dot-mode
(require 'graphviz-dot-mode)


;;; highlight-indent-guides

(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'bitmap
      highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-dots
      highlight-indent-guides-responsive nil
      highlight-indent-guides-delay 0
      highlight-indent-guides-auto-character-face-perc 20
      highlight-indent-guides-auto-top-character-face-perc 40
      highlight-indent-guides-auto-stack-character-face-perc 30)
(keymap-global-set "C-c h i" #'highlight-indent-guides-mode)


;;; highlight-numbers

(keymap-global-set "C-c h n" #'highlight-numbers-mode)


;;; hippie-expand

(setq my-he-try-functions-dabbrev '(try-expand-dabbrev
                                    try-expand-dabbrev-visible
                                    try-expand-dabbrev-all-buffers
                                    try-expand-dabbrev-from-kill
                                    try-expand-all-abbrevs)
      my-he-try-functions-line '(try-expand-line
                                 try-expand-line-all-buffers)
      my-he-try-functions-filename '(try-complete-file-name-partially
                                     try-complete-file-name))

;; Combine all the above as the default behavior for `hippie-expand'
(setq hippie-expand-try-functions-list (append
                                        my-he-try-functions-dabbrev
                                        my-he-try-functions-line
                                        my-he-try-functions-filename))
(fset 'hippie-expand-filename-first
      (make-hippie-expand-function (append my-he-try-functions-filename
                                           my-he-try-functions-dabbrev
                                           my-he-try-functions-line)
                                   t))
(fset 'hippie-expand-line-first
      (make-hippie-expand-function (append my-he-try-functions-line
                                           my-he-try-functions-filename
                                           my-he-try-functions-dabbrev)
                                   t))

(defun hippie-expand-disable-minor-modes ()
  (interactive)
  (hippie-expand-line-mode -1)
  (hippie-expand-filename-mode -1))

(define-minor-mode hippie-expand-filename-mode
  "Modify hippie-expand to prioritize filename expansions."
  :global t
  :init-value nil
  :lighter " HippieFilename"
  :keymap
  '(([?\M-/] . hippie-expand-filename-first)
    ([?\C-g] . (lambda ()
                 (interactive)
                 (hippie-expand-disable-minor-modes)
                 (keyboard-quit))))
  (when hippie-expand-filename-mode
    (hippie-expand-line-mode -1)))

(define-minor-mode hippie-expand-line-mode
  "Modify hippie-expand to prioritize line expansions."
  :global t
  :init-value nil
  :lighter " HippieLine"
  :keymap
  '(([?\M-/] . hippie-expand-line-first)
    ([?\C-g] . (lambda ()
                 (interactive)
                 (hippie-expand-disable-minor-modes)
                 (keyboard-quit))))
  (when hippie-expand-line-mode
    (hippie-expand-filename-mode -1)))

(keymap-global-set "M-/" #'hippie-expand)
(keymap-global-set "C-c e f" #'hippie-expand-filename-mode)
(keymap-global-set "C-c e l" #'hippie-expand-line-mode)


;;; howm

(require 'org)
;; (require 'howm-org) I don't like using org-style timestamps within howm

(setq howm-default-key-table nil
      howm-excluded-dirs '("data" "RCS" "CVS" ".svn" ".git" "_darcs")
      howm-list-title-regexp "^(\\*$|(\\*|#\\+title:) +)" ; passed to grep/rg
      howm-menu-file-extension ".org"
      howm-prefix nil
      howm-view-title-header "*"
      howm-wiki-regexp nil)

(require 'howm)
(require 'howm-attach)
(require 'howm-shift)

(setq *howm-show-item-filename* nil  ;; don't show filenames in the echo area when browsing notes
      howm-action-lock-forward-save-buffer t
      howm-check-word-break t ;; keywords starting with @#*-+/~ won't be marked (only %$ work)
      howm-content-from-region t
      howm-directory "~/howm/"
      howm-file-name-format "%Y/%m/%Y-%m-%d.org"
      ;; include timestamp part and note filenames
      howm-highlight-date-regexp-format "\\(?:\\[%Y-%m-%d.*?]\\|%Y-%m-%d\\)?"
      howm-history-file (expand-file-name ".howm-history" howm-directory)
      howm-history-limit nil
      howm-iigrep-preview-items 50
      howm-keyword-case-fold-search t
      howm-keyword-file (expand-file-name ".howm-keys" howm-directory)
      howm-list-recent-days 14
      howm-menu-file (expand-file-name "howm-menu.org" user-emacs-directory)
      howm-menu-footer ""
      howm-menu-name-format "*howm-menu:%s*"
      howm-menu-recent-num 20
      howm-menu-schedule-days 30
      howm-menu-schedule-days-before 14
      howm-menu-todo-num 50
      ;; howm-menu-todo-priority-format nil;;"(%8.1f)"
      howm-message-time nil
      howm-normalizer 'howm-sort-items-by-mtime
      howm-prepend t
      howm-user-font-lock-keywords '(("keywords:" . (0 'howm-mode-ref-face)))
      howm-view-contents-name "*howm-contents:%s*"
      howm-view-grep-command "rg"
      howm-view-grep-expr-option "-e"
      howm-view-grep-extended-option nil
      howm-view-grep-file-stdin-option "-f -"
      howm-view-grep-fixed-option "-F"
      howm-view-grep-option "-nH --no-heading --color never -g !data/"
      howm-view-keep-one-window t
      howm-view-split-horizontally nil
      howm-view-summary-format (let* ((path (format-time-string howm-file-name-format))
                                      (width (length (file-name-nondirectory path)))
                                      (max-width (int-to-string 30)))
                                 (concat "%-" max-width "." max-width "s" howm-view-summary-sep " "))
      howm-view-summary-name "*howm-summary*"
      howm-view-summary-window-size 20
      howm-view-use-grep t)

;; When titles are shown, it seems the  ordering (howm-normalize) sometimes is not followed?
;; (add-to-list 'howm-list-title 'howm-list-grep-fixed)
;; (add-to-list 'howm-list-title 'howm-keyword-search)

(add-to-list 'howm-template-rules
             '("%dateonly" . (lambda (arg)
                               (let ((date (format-time-string howm-date-format)))
                                 (insert (format howm-insert-date-format date))))))

(defun my-howm-template (which-template previous-buffer)
  "Howm template chooser."
  (let ((templates `(("default"
                      ,(concat howm-view-title-header " %title\n%date %file\n\n%cursor\n\n"))

                     ("Meeting"
                      ,(concat howm-view-title-header " Meeting: %title%cursor\n"
                               "%date %file\n\n"))

                     ("Meeting - Standup/DSM"
                      ,(concat howm-view-title-header " Meeting: Standup\n"
                               "%date\n\n"
                               "%cursor\n\n"))

                     ("Task log"
                      ,(concat howm-view-title-header " Task Log\n"
                               "%date\n\n"
                               "tasklog%cursor\n\n")))))
    (cond
     ((= which-template 2)
      (concat howm-view-title-header " %title\n%date %file\n\n%cursor\n\n"))
     ((= which-template 4)
      (let ((choice (completing-read "Template: "
                                     (my-presorted-completion-table (mapcar #'car templates))
                                     nil t nil t)))
        (cadr (assoc choice templates))))
     (t
      (concat howm-view-title-header " %title\n%date\n\n%cursor\n\n")))))

(setq howm-template #'my-howm-template)

(defun my-howm-insert-keywords-line ()
  "Insert keywords line."
  (interactive)
  (let* ((completion-table (mapcar #'list (howm-keyword-list)))
         (keywords (completing-read-multiple "Keyword: " completion-table)))
    (unless (eolp) (goto-char (pos-eol)))
    (newline)
    (insert "keywords: " (string-join keywords " "))))

(defun my-howm-insert-ref ()
  "Insert a ref or goto-link to a file."
  (interactive)
  (insert howm-ref-header
          " "
          (abbreviate-file-name (read-file-name "Insert ref to: " (concat howm-directory "data/")))))

(defun my-howm-collect-keywords ()
  "Write all keywords found in the current buffer."
  (interactive)
  (let* ((all-keywords)
         (org-regexp (concat "\\(?:"
                             "#\\+title: \\(.+\\)$"
                             "\\|"
                             "#\\+filetags: \\(.+\\)$"
                             "\\|"
                             "keywords: \\(.+\\)$"
                             "\\)"))
         (text-regexp (concat "\\(?:"
                              "keywords: \\(.+\\)$"
                              "\\)")))
    (save-excursion
      (goto-char (point-min))
      (cond
       ((eq major-mode 'org-mode)
        (while (re-search-forward org-regexp nil t)
          (let ((org-title (match-string-no-properties 1))
                (org-filetags (match-string-no-properties 2))
                (keywords-line (match-string-no-properties 3)))
            (when org-title
              (setq all-keywords (append all-keywords
                                         (list org-title))))
            (when org-filetags
              (setq all-keywords (append all-keywords
                                         (split-string org-filetags ":" t))))
            (when keywords-line
              (setq all-keywords (append all-keywords
                                         (split-string keywords-line " ")))))))

       ((derived-mode-p 'text-mode)
        (while (re-search-forward text-regexp nil t)
          (let ((keywords-line (match-string-no-properties 1)))
            (when keywords-line
              (setq all-keywords (append all-keywords
                                         (split-string keywords-line " "))))))))

      (let ((save-silently t))
        (howm-keyword-add all-keywords)))))

(defun my-howm-insert-before-symbol (header)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (bounds-start (car bounds))
         (bounds-end (cdr bounds)))
    (when (and bounds
               (> (point) bounds-start)
               (< (point) bounds-end))
      (goto-char bounds-start))
    (unless (string-match-p "[ \t\n]" (char-to-string (char-before (point))))
      (insert " "))
    (insert header)
    (when (or (eobp)
              (not (equal " " (char-to-string (char-after (point))))))
      (insert " "))))

(defun my-howm-insert-ref-header ()
  (interactive)
  (my-howm-insert-before-symbol howm-ref-header)
  (howm-insert-keyword))

(defun my-howm-insert-keyword-header ()
  (interactive)
  (my-howm-insert-before-symbol howm-keyword-header)
  (howm-insert-keyword))

(defun my-howm-list-grep-contents ()
  (interactive)
  (when (howm-list-grep t)
    (riffle-summary-to-contents)))

(defun my-howm-create-other-window (&optional which-template here)
  (interactive "p")
  (split-window-sensibly)
  (other-window 1)
  (howm-create which-template here))

(defun my-howm-mode-keys ()
  (keymap-set howm-mode-map "S-<down>" #'howm-shift-down)
  (keymap-set howm-mode-map "S-<left>" #'howm-shift-left)
  (keymap-set howm-mode-map "S-<right>" #'howm-shift-right)
  (keymap-set howm-mode-map "S-<up>" #'howm-shift-up)
  (keymap-set howm-mode-map
              "C-z" (let ((map (make-sparse-keymap)))
                      (keymap-set map "." #'howm-find-today)
                      (keymap-set map "1" #'howm-list-schedule)
                      (keymap-set map "2" #'howm-list-todo)
                      (keymap-set map ":" #'howm-find-yesterday)
                      (keymap-set map "A" #'howm-list-around)
                      (keymap-set map "C" #'howm-create-here)
                      (keymap-set map "D" #'howm-dup)
                      (keymap-set map "F" #'howm-first-memo)
                      (keymap-set map "I" #'howm-create-interactively)
                      (keymap-set map "K" #'howm-keyword-to-kill-ring)
                      (keymap-set map "L" #'howm-last-memo)
                      (keymap-set map "M" #'howm-open-named-file)
                      (keymap-set map "N" #'howm-next-memo)
                      (keymap-set map "P" #'howm-previous-memo)
                      (keymap-set map "Q" #'howm-kill-all)
                      (keymap-set map "S" #'howm-search-past)
                      (keymap-set map "SPC" #'howm-toggle-buffer)
                      (keymap-set map "[" #'my-org-inactive-timestamp)
                      (keymap-set map "a" #'howm-list-all)
                      (keymap-set map "c" #'howm-create)
                      (keymap-set map "d" #'howm-insert-date)
                      (keymap-set map "e" #'howm-remember)
                      (keymap-set map "g" #'howm-list-grep)
                      (keymap-set map "h" #'howm-history)
                      (keymap-set map "i" #'howm-insert-keyword)
                      (keymap-set map "k" #'my-howm-insert-keywords-line)
                      (keymap-set map "l" #'howm-list-recent)
                      (keymap-set map "m" #'howm-menu)
                      (keymap-set map "n" #'action-lock-goto-next-link)
                      (keymap-set map "o" #'howm-occur)
                      (keymap-set map "p" #'action-lock-goto-previous-link)
                      (keymap-set map "r" #'howm-refresh)
                      (keymap-set map "s" #'howm-list-grep-fixed)
                      (keymap-set map "t" #'howm-insert-dtime)
                      (keymap-set map "w" #'howm-toggle-narrow)
                      (keymap-set map "x" #'howm-list-mark-ring)
                      (keymap-set map ">" #'my-howm-insert-ref)
                      (keymap-set map "C-." #'my-howm-insert-ref-header)
                      (keymap-set map "C-," #'my-howm-insert-keyword-header)
                      (keymap-set map "C-a" #'howm-attach)
                      map)))

(defun my-howm-other-modes-keys ()
  (mapc (lambda (map)
          (keymap-set map "." #'howm-reminder-goto-today)
          (keymap-set map "1" #'howm-list-schedule)
          (keymap-set map "2" #'howm-list-todo)
          (keymap-set map "K" #'howm-keyword-to-kill-ring)
          (keymap-set map "M" #'howm-open-named-file)
          (keymap-set map "O" #'my-howm-create-other-window)
          (keymap-set map "Q" #'howm-kill-all)
          (keymap-set map "s" #'howm-list-grep-fixed)
          (keymap-set map "a" #'howm-list-all)
          (keymap-set map "c" #'howm-create)
          (keymap-set map "e" #'howm-remember)
          (keymap-set map "m" #'howm-menu)
          (keymap-set map "o" #'howm-occur)
          (keymap-set map "g" #'howm-list-grep)
          (keymap-set map "x" #'howm-list-mark-ring))
        (list howm-view-summary-mode-map
              howm-view-contents-mode-map)))

(defun my-howm-mode-config ()
  (setq-local fill-column 100)
  (visual-line-fill-column-mode t))

(defun my-howm-before-save ()
  (my-howm-collect-keywords)
  ;; ensure there is a trailing space at the end of headers
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+$" nil t)
      (replace-match (concat (match-string 0) " ")))))

(defun my-howm-message-title-state (&optional undo)
  (message "Titles: %s" (if howm-list-title-previous "On" "Off")))

(advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
(advice-add 'howm-list-toggle-title :after #'my-howm-message-title-state)

(add-hook 'howm-create-hook #'howm-narrow-to-memo)
(add-hook 'howm-mode-hook #'my-howm-mode-config)
(add-hook 'howm-mode-hook #'my-howm-mode-keys)
(add-hook 'howm-view-contents-mode-hook #'howm-mode)
(add-hook 'howm-view-contents-mode-hook #'howm-org-font-lock-minor-mode)
(add-hook 'howm-view-contents-mode-hook #'my-howm-mode-config)
(add-hook 'howm-view-contents-mode-hook #'my-howm-mode-keys)
(add-hook 'howm-view-contents-mode-hook #'my-howm-other-modes-keys)
(add-hook 'howm-view-open-hook #'howm-narrow-to-memo)
(add-hook 'howm-view-summary-mode-hook #'my-howm-other-modes-keys)
;; Make sure this runs late to make `delete-trailing-whitespace' not remove the trailing header spaces
(add-hook 'howm-mode-hook (lambda () (add-hook 'before-save-hook #'my-howm-before-save 90 t)))
(add-hook 'howm-view-summary-mode-hook #'hl-line-mode)

(keymap-set howm-menu-mode-map "<backtab>" #'action-lock-goto-previous-link)
(keymap-set howm-menu-mode-map "n" #'next-line)
(keymap-set howm-menu-mode-map "p" #'previous-line)
(keymap-set howm-view-summary-mode-map "<backtab>" #'howm-view-summary-previous-section)
(keymap-set howm-view-contents-mode-map "<backtab>" #'riffle-contents-goto-previous-item)
(keymap-global-set "C-z ." #'howm-find-today)
(keymap-global-set "C-z 1" #'howm-list-schedule)
(keymap-global-set "C-z 2" #'howm-list-todo)
(keymap-global-set "C-z :" #'howm-find-yesterday)
(keymap-global-set "C-z @" #'my-howm-list-grep-contents)
(keymap-global-set "C-z D" #'howm-dup)
(keymap-global-set "C-z H" #'howm-mode)
(keymap-global-set "C-z I" #'howm-create-interactively)
(keymap-global-set "C-z K" #'howm-keyword-to-kill-ring)
(keymap-global-set "C-z M" #'howm-open-named-file)
(keymap-global-set "C-z S" #'howm-search-past)
(keymap-global-set "C-z SPC" #'howm-toggle-buffer)
(keymap-global-set "C-z a" #'howm-list-all)
(keymap-global-set "C-z c" #'howm-create)
(keymap-global-set "C-z d" #'howm-insert-date)
(keymap-global-set "C-z e" #'howm-remember)
(keymap-global-set "C-z g" #'howm-list-grep)
(keymap-global-set "C-z h" #'howm-history)
(keymap-global-set "C-z i" #'howm-insert-keyword)
(keymap-global-set "C-z l" #'howm-list-recent)
(keymap-global-set "C-z m" #'howm-menu)
(keymap-global-set "C-z s" #'howm-list-grep-fixed)
(keymap-global-set "C-z t" #'howm-insert-dtime)
(keymap-global-set "C-z w" #'howm-random-walk)


;;; howm (action-lock)

(require 'action-lock)

(setq my-action-lock-checkbox '("[ ]" "[X]" "[-]" "[*]")
      my-action-lock-datebox '("[@]" "[%Y-%m-%d %H:%M]")
      my-action-lock-jira-issue-regexp "\\(JIRA-[0-9]+\\)")

(defun my-action-lock-jira-browse (issue)
  "action-lock to browse a Jira issue."
  (browse-url (format "https://JIRA.atlassian.net/browse/%s" issue)))

(defun my-action-lock-jira-browse-rule (regexp arg-pos &optional hilit-pos)
  "action-lock rule to browse a Jira issue."
  (action-lock-general #'my-action-lock-jira-browse regexp arg-pos hilit-pos))

(defun my-action-lock-mode-config ()
  (when action-lock-mode
    (action-lock-add-rules
     (list (action-lock-switch my-action-lock-checkbox)
           (action-lock-date (regexp-quote (car my-action-lock-datebox))
                             (cadr my-action-lock-datebox))
           (my-action-lock-jira-browse-rule my-action-lock-jira-issue-regexp 0)))))

(add-hook 'action-lock-mode-hook #'my-action-lock-mode-config)


;;; ibuffer

(require 'ibuffer)
(require 'ibuf-ext)
(setq ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups
      '(("default"
         ("Emacs Lisp" (mode . emacs-lisp-mode))
         ("Python" (mode . python-mode))
         ("Javascript" (mode . js2-mode))
         ("YAML" (mode . yaml-mode))
         ("Prodigy" (or (mode . prodigy-mode)
                        (mode . prodigy-view-mode)))
         ("Shell/Term" (or (mode . shell-mode)
                           (mode . eshell-mode)
                           (mode . term-mode)))
         ("Kotlin" (mode . kotlin-mode))
         ("Nix" (mode . nix-mode))
         ("Org" (mode . org-mode))
         ("Dired" (mode . dired-mode))
         ("Magit" (name . "^magit.*:"))
         ("xref" (name . "^\\*xref\\*$"))
         ("emacs" (or
                   (name . "^\\*Customize\\*$")
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*Backtrace\\*$")
                   (name . "^\\*Help\\*$")
                   (name . "^\\*RE-Builder\\*$")
                   (name . "^\\*Async-native-compile-log\\*$")
                   (name . "^\\*Packages\\*$")
                   (name . "^\\*Alerts\\*$")))
         ("Mail" (or (mode . message-mode)
                     (name . "^\\*sent mail")
                     (name . "^\\*unsent mail")))
         ("EMMS" (or (mode . emms-playlist-mode)
                     (mode . emms-browser-mode)
                     (mode . emms-show-all-mode))))))
(add-hook 'ibuffer-mode-hook (lambda ()
                               (ibuffer-switch-to-saved-filter-groups "default")))
(add-hook 'ibuffer-mode-hook #'hl-line-mode)
(keymap-global-set "C-x C-b" 'ibuffer)


;;; icomplete

(with-eval-after-load 'orderless
  (require 'icomplete)
  (setq icomplete-prospects-height 1
        icomplete-separator (propertize "  |  " 'face 'font-lock-variable-name-face)
        icomplete-compute-delay 0
        completion-auto-help t
        completion-cycle-threshold nil
        completion-pcm-complete-word-inserts-delimiters t)

  (defun space-dash-star ()
    "Cycle the previous character between dash, asterisk, and SPC characters."
    (interactive)
    (let ((prev-char (buffer-substring (- (point) 1) (point)))
          (replace-prev-char (lambda (char) (delete-char -1) (insert char))))
      (cond
       ((equal prev-char " ") (funcall replace-prev-char "-"))
       ((equal prev-char "-") (funcall replace-prev-char "*"))
       ((equal prev-char "*") (funcall replace-prev-char " "))
       (t (insert " ")))))

  (defun minibuffer-selection-kill-ring-save (arg)
    "Save the minibuffer selection to the kill ring, appending if ARG is non-nil."
    (interactive "P")
    (if (not (active-minibuffer-window))
        (message "The minibuffer is not active")
      (let ((selection (car (completion-all-sorted-completions))))
        (if arg (kill-append (concat "\n" selection) nil)
          (kill-new selection)))
      (message "Selection copied")))

  (cond
   ((>= emacs-major-version 27)
    ;; fido-mode matches most of my icomplete preferences and it also
    ;; sets the same vars in the `t' condition

    (if (= emacs-major-version 27)
        (fido-mode t)
      ;; Emacs 28 introduces `fido-vertical-mode'
      (fido-vertical-mode t))

    (defun my-icomplete-config ()
      (setq-local max-mini-window-height 0.15
                  ;; Setting the completion-styles here is necessary
                  ;; because `icomplete--fido-mode-setup' sets it to
                  ;; flex by force.
                  completion-styles '(orderless basic)

                  ;; Completion falls back to using completion-styles if
                  ;; completion-category-overrides doesn't yield a
                  ;; result
                  completion-category-overrides '((file (styles . (basic flex partial-completion)))
                                                  (buffer (styles . (basic flex partial-completion)))))
      (keymap-set icomplete-minibuffer-map "C-?" #'minibuffer-hide-completions)
      (keymap-set icomplete-minibuffer-map "C-S-j" #'icomplete-force-complete)
      (keymap-set icomplete-minibuffer-map "C-<return>" #'icomplete-force-complete)
      (keymap-set icomplete-minibuffer-map "C-^" (lambda ()
                                                   (interactive)
                                                   (setq-local max-mini-window-height
                                                               (if (< max-mini-window-height 0.3) 0.3 0.75))))
      (keymap-set icomplete-minibuffer-map "C-c M-w" #'minibuffer-selection-kill-ring-save)
      (keymap-set icomplete-minibuffer-map "C-n" #'icomplete-forward-completions)
      (keymap-set icomplete-minibuffer-map "C-p" #'icomplete-backward-completions)
      (keymap-set icomplete-minibuffer-map "S-SPC" (lambda ()
                                                     (interactive)
                                                     (self-insert-command 1 ? )))
      (keymap-set icomplete-minibuffer-map "SPC" #'space-dash-star))
    (add-hook 'icomplete-minibuffer-setup-hook #'my-icomplete-config))

   (t
    ;; Original preferences prior to Emacs 27
    (require 'icomplete-emacs-27)

    (defun star-before-word-completion ()
      "Insert a literal `*' on the first invocation, then runs minibuffer-complete-word if invoked again.
Useful for completion style 'partial-completion."
      (interactive)
      (let ((prev-char (buffer-substring (- (point) 1) (point))))
        (if (or (equal prev-char "-") (equal prev-char "*"))
            (minibuffer-complete-word)
          (insert-char ?*))))

    (setq icomplete-hide-common-prefix nil
          icomplete-show-matches-on-no-input t
          completion-ignore-case t
          read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t)
    (let ((map icomplete-minibuffer-map))
      (keymap-set icomplete-minibuffer-map "C-S-j" #'minibuffer-force-complete)
      (keymap-set icomplete-minibuffer-map "SPC" #'star-before-word-completion))
    (icomplete-mode t))))


;;; ido

(skipped
 (require 'ido)
 (setq confirm-nonexistent-file-or-buffer nil
       ido-auto-merge-delay-time 0.3
       ido-completion-buffer nil
       ido-create-new-buffer 'always
       ido-decorations '(" { "
                         " } "
                         " | "
                         " | ..."
                         "["
                         "]"
                         " [No match]"
                         " [Matched]"
                         " [Not readable]"
                         " [Too big]"
                         " [Confirm]")
       ido-max-prospects 10
       ido-max-window-height 1
       ido-use-virtual-buffers t
       ido-enable-flex-matching 1)
 (add-hook 'ido-minibuffer-setup-hook (lambda ()
                                        (visual-line-mode 1))))


;;; image-dired

(with-eval-after-load 'dired
  (setq image-dired-marking-shows-next nil
        image-dired-thumb-margin 10
        image-dired-thumb-relief 1
        image-dired-thumbnail-storage 'standard)
  (keymap-set dired-mode-map "C-t ." (defun image-dired-current-directory ()
                                       (interactive)
                                       (image-dired dired-directory)))
  (keymap-set dired-mode-map "C-t 0" (defun image-dired-standard-thumbnails ()
                                       (interactive)
                                       (setq image-dired-thumbnail-storage 'standard)))
  (keymap-set dired-mode-map "C-t 1" (defun image-dired-large-thumbnails ()
                                       (interactive)
                                       (setq image-dired-thumbnail-storage 'standard-large)))
  (keymap-set dired-mode-map "C-t 2" (defun image-dired-x-large-thumbnails ()
                                       (interactive)
                                       (setq image-dired-thumbnail-storage 'standard-x-large)))
  (keymap-set dired-mode-map "C-t 3" (defun image-dired-xx-large-thumbnails ()
                                       (interactive)
                                       (setq image-dired-thumbnail-storage 'standard-xx-large)))
  (defun my-image-dired-config ()
    (let ((map image-dired-thumbnail-mode-map))
      (keymap-set map "<tab>" #'image-dired-forward-image)
      (keymap-set map "<backtab>" #'image-dired-backward-image)
      (keymap-set map "f" #'image-dired-forward-image)
      (keymap-set map "b" #'image-dired-backward-image)
      (keymap-set map "n" #'image-dired-next-line)
      (keymap-set map "p" #'image-dired-previous-line)))
  (add-hook 'image-dired-thumbnail-mode-hook #'my-image-dired-config))

;;; imenu

(defun my-imenu-emacs-lisp-mode ()
  (add-to-list 'imenu-generic-expression '("Sections" "^;;; \\(.*$\\)" 1))
  (add-to-list 'imenu-generic-expression '("Subsections" "^;;;; \\(.*$\\)" 1)))
(defun my-imenu-restclient-mode ()
  (setq imenu-generic-expression '((nil "^#+ *\\(.+\\)\n[A-Z]+ " 1)
                                   ("Verb DELETE" "^\\(DELETE \\).*$" 0)
                                   ("Verb PUT" "^\\(PUT \\).*$" 0)
                                   ("Verb POST" "^\\(POST\\).*$" 0)
                                   ("Verb GET" "^\\(GET \\).*$" 0)
                                   ("Sections" "^\n#+ *\\(.*\\)\n$" 1))))
(add-hook 'emacs-lisp-mode-hook #'my-imenu-emacs-lisp-mode)
(add-hook 'restclient-mode-hook #'my-imenu-restclient-mode)


;;; imenu-list

(require 'imenu-list)
(setq imenu-list-auto-resize nil
      imenu-list-focus-after-activation t
      imenu-list-position 'left
      imenu-list-size 0.2)

(defun imenu-list-ret-dwim-then-quit ()
  "Quit the `imenu-list' window after selection."
  (interactive)
  (imenu-list-ret-dwim)
  (imenu-list-quit-window))

(add-hook 'imenu-list-major-mode-hook #'hl-line-mode)
(keymap-global-set "C-c i m" #'imenu-list-minor-mode)
(keymap-set imenu-list-major-mode-map "<return>" #'imenu-list-ret-dwim-then-quit)
(keymap-set imenu-list-major-mode-map "C-<return>" #'imenu-list-ret-dwim)
(keymap-set imenu-list-major-mode-map "s" #'isearch-forward)


;;; inf-clojure

(require 'inf-clojure)
(setq inf-clojure-custom-repl-type 'cljs)
(setq inf-clojure-custom-startup "clojure -m cljs.main -r")
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
(add-hook 'clojurescript-mode-hook #'inf-clojure-minor-mode)


;;; isearch

(require 'isearch)
(setq isearch-allow-scroll 'unlimited
      isearch-lazy-count t
      search-whitespace-regexp "[ \t\r\n]+")

(defun my-isearch-control-g ()
  "Skip the rub out behavior and cancel the current isearch."
  (interactive)
  (setq isearch-success nil)
  (isearch-cancel))

(defun my-isearch-mode-config ()
  (keymap-set isearch-mode-map "C-g" #'my-isearch-control-g)
  (keymap-set isearch-mode-map "C-`" #'other-window))
(add-hook 'isearch-mode-hook #'my-isearch-mode-config)


;;; ivy

(skipped
 (require 'ivy)
 (ivy-mode 1)
 (setq ivy-count-format ""
       ivy-height 6
       ivy-use-virtual-buffers 1
       ivy-wrap 1)
 (keymap-global-set "M-x" 'counsel-M-x)
 ;; (keymap-global-set "C-x r b" 'counsel-bookmark)
 (keymap-global-set "C-x C-f" 'counsel-find-file)
 (keymap-global-set "C-h C-l" 'counsel-find-library)
 (keymap-global-set "C-h f" 'counsel-describe-function)
 (keymap-global-set "C-h v" 'counsel-describe-variable)
 (keymap-global-set "C-h C-u" 'counsel-unicode-char))


;;; js

(setq js-indent-level 2
      js-switch-indent-offset 2)
(add-hook 'js2-mode-hook #'eglot-ensure)


;;; js2-mode

(require 'js2-mode)
(setq js2-strict-missing-semi-warning nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun my-js2-mode-config ()
  (keymap-set js2-mode-map "C-M-SPC" #'js2-mark-parent-statement)
  (keymap-set js2-mode-map "C-M-h" #'js2-mark-defun)
  (keymap-set js2-mode-map "C-c m j" #'js2-print-json-path)
  (keymap-set js2-mode-map "<return>" #'js2-line-break)
  (keymap-set js2-mode-map "C-k" #'js2r-kill)
  ;; Let xref-js2 handle references and definitions
  (keymap-unset js2-mode-map "M-."))

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'no-indent-tabs-mode)
(add-hook 'js2-mode-hook #'subword-mode)
(add-hook 'js2-mode-hook #'my-js2-mode-config)


;;; js2-refactor

(with-eval-after-load 'js2-mode
  (require 'js2-refactor)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))


;;; json-mode

(defun my-json-mode-config ()
  (keymap-set json-mode-map "C-c m j" #'jsons-print-path))
(add-hook 'json-mode-hook #'my-json-mode-config)


;;; json-navigator

(require 'json-navigator)
(require 'tree-mode)
(setq json-navigator-display-length 10)
(defun my-json-navigator-mode-config ()
  (keymap-set json-navigator-mode-map "*" #'tree-mode-expand-level)
  (keymap-set json-navigator-mode-map "SPC" #'tree-mode-toggle-expand)
  (keymap-set json-navigator-mode-map "^" #'tree-mode-goto-parent)
  (keymap-set json-navigator-mode-map "n" #'widget-forward)
  (keymap-set json-navigator-mode-map "p" #'widget-backward))
(add-hook 'json-navigator-mode-hook #'my-json-navigator-mode-config)


;;; kotlin-mode

(add-hook 'kotlin-mode-hook #'flycheck-mode)


;;; latex

(setq latex-run-command "pdflatex"
      tex-dvi-view-command "emacsclient -e \"(find-file-other-window \\\"*\\\")\""
      tex-print-file-extension ".pdf"
      tex-run-command "pdflatex")
(defun tex-save-then-file (&optional arg)
  "Save the file, except if an ARG was passed, then run `tex-file'."
  (interactive "P")
  (unless arg
    (save-buffer))
  (call-interactively #'tex-file))
(defun my-latex-mode-config ()
  "LaTeX mode config."
  (keymap-set latex-mode-map "C-c C-f" #'tex-save-then-file))
(add-hook 'latex-mode-hook #'my-latex-mode-config)
;; (add-hook 'LaTeX-mode-hook (lambda()
;;                              (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))


;;; magit

(require 'magit)
(setq magit-commit-ask-to-stage 'stage
      magit-diff-refine-hunk t
      magit-status-goto-file-position t
      magit-status-show-hashes-in-headers t)

(defun magit-commit-create-updates ()
  (interactive)
  (magit-commit-create '("-m" "updates")))

(transient-append-suffix 'magit-commit "c"
  '("C" "Commit generic" magit-commit-create-updates))

(keymap-global-set "C-c g M-g" #'magit-dispatch)
(keymap-global-set "C-c g g" #'magit-status)
(keymap-global-set "C-c g u" #'magit-ediff-show-unstaged)


;;; mail

(setq smtpmail-smtp-server "127.0.0.1"
      smtpmail-smtp-service 1025
      smtpmail-stream-type 'starttls
      ;; smtpmail-servers-requiring-authorization "127.0.0.1"
      send-mail-function 'sendmail-send-it
      sendmail-program "msmtp")


;;; marginalia

(require 'marginalia)
(keymap-set minibuffer-local-map "C-c m m" #'marginalia-cycle)
(marginalia-mode)


;;; markdown

(require 'markdown-mode)
(setq markdown-asymmetric-header t
      markdown-make-gfm-checkboxes-buttons nil)
(with-eval-after-load 'markdown-mode
  (keymap-set markdown-mode-map "C-<return>" #'markdown-insert-header-dwim)
  (keymap-set markdown-mode-map "M-<left>" #'markdown-promote)
  (keymap-set markdown-mode-map "M-<right>" #'markdown-demote))
(defun my-markdown-config ()
  "`markdown-mode' config."
  (indent-tabs-mode -1)
  (setq fill-column 80
        markdown-unordered-list-item-prefix "  * "))
(add-hook 'markdown-mode-hook #'my-markdown-config)
(add-hook 'markdown-mode-hook #'visual-line-fill-column-mode)


;;; mermaid-mode

(require 'mermaid-mode)
(require 'mermaid-ts-mode)
(setq mermaid-ts-indent-level 4
      mermaid-output-format ".png"
      mermaid-flags "-s 3")
(add-to-list 'auto-mode-alist '("\\.mermaid\\'" . mermaid-mode))
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))

(defun my-mermaid-config ()
  (setq-local indent-tabs-mode nil))

(defun my-mermaid-compile-svg ()
  (interactive)
  (save-buffer)
  (let* ((mermaid-output-format ".svg")
         (input (buffer-file-name))
         (output (concat (file-name-sans-extension input) mermaid-output-format))
         (exit-code (apply #'call-process
                           mermaid-mmdc-location nil "*mmdc*" nil (append (split-string mermaid-flags " ")
                                                                          (list "-i" input "-o" output)))))
    (when (zerop exit-code)
      (shell-command (concat "open " (shell-quote-argument output))))))

(keymap-set mermaid-mode-map "C-c C-b" #'mermaid-compile-buffer)
(keymap-set mermaid-mode-map "C-c C-d" #'mermaid-open-doc)
(keymap-set mermaid-mode-map "C-c C-f" #'mermaid-compile-file)
(keymap-set mermaid-mode-map "C-c C-o" #'mermaid-open-browser)
(keymap-set mermaid-mode-map "C-c C-r" #'mermaid-compile-region)
(keymap-set mermaid-mode-map "C-c C-c" (lambda ()
                                         (interactive)
                                         (save-buffer)
                                         (mermaid-compile)))
(keymap-set mermaid-mode-map "C-c C-s" #'my-mermaid-compile-svg)
(add-hook 'mermaid-mode-hook #'my-mermaid-config)


;;; misearch

(keymap-global-set "C-c d M-%" #'replace-regexp-as-diff)


;;; multi-term

(require 'multi-term)
(setq multi-term-program "/run/current-system/sw/bin/bash"
      term-bind-key-alist '(("C-c C-c" . term-interrupt-subjob)
                            ("C-c C-j" . term-line-mode)
                            ("C-p" . previous-line)
                            ("C-n" . next-line)
                            ("C-s" . isearch-forward)
                            ("C-r" . isearch-backward)
                            ("C-m" . term-send-return)
                            ("C-y" . term-paste)
                            ("M-f" . term-send-forward-word)
                            ("M-b" . term-send-backward-word)
                            ("M-o" . term-send-backspace)
                            ("M-p" . term-send-up)
                            ("M-n" . term-send-down)
                            ("M-DEL" . term-send-backward-kill-word)
                            ("<C-backspace>" . term-send-backward-kill-word)
                            ("M-r" . term-send-reverse-search-history)
                            ("M-d" . term-send-delete-word)
                            ("M-," . term-send-raw)
                            ("M-." . comint-dynamic-complete)))
(keymap-global-set "C-c t T" #'multi-term)
(keymap-global-set "C-c t t" #'multi-term-next)


;;; multiple-cursors

(require 'multiple-cursors)
(keymap-global-set "C-c m c ." #'mc/mark-all-like-this-dwim)
(keymap-global-set "C-c m c C-M-SPC" #'mc/mark-all-in-region-regexp)
(keymap-global-set "C-c m c C-SPC" #'mc/mark-all-in-region)
(keymap-global-set "C-c m c C-a" #'mc/edit-beginnings-of-lines)
(keymap-global-set "C-c m c C-e" #'mc/edit-ends-of-lines)
(keymap-global-set "C-c m c C-n" #'mc/mark-next-like-this)
(keymap-global-set "C-c m c C-p" #'mc/mark-previous-like-this)
(keymap-global-set "C-c m c e" #'mc/edit-lines)
(keymap-global-set "C-c m c i l" #'mc/insert-letters)
(keymap-global-set "C-c m c i n" #'mc/insert-numbers)
(keymap-set mc/keymap "C-c ," #'mc/mark-previous-like-this)
(keymap-set mc/keymap "C-c ." #'mc/mark-next-like-this)
(keymap-set mc/keymap "C-c <" #'mc/skip-to-previous-like-this)
(keymap-set mc/keymap "C-c >" #'mc/skip-to-next-like-this)

(defun my-multiple-cursors-config ()
  "`multiple-cursors-mode' config."
  (keymap-unset mc/keymap "<return>"))  ; Enable <return> key while on multiple cursors
(add-hook 'multiple-cursors-mode-hook #'my-multiple-cursors-config)


;;; native-complete

(skipped
 ;; Put HISTCONTROL=ignoreboth in bashrc to avoid polluting the shell
 ;; history with the `echo' commands made by this package
 (with-eval-after-load 'shell
   (require 'native-complete)
   (native-complete-setup-bash)))


;;; nav-flash

(require 'nav-flash)
(add-hook 'occur-mode-find-occurrence-hook #'nav-flash-show)


;;; newsticker

(skipped
 (require 'newsticker)
 (defun my-newsticker-custom-keys ()
   (keymap-set newsticker-mode-map "<tab>" 'newsticker-show-entry))
 (defun newsticker--cache-update-advice (orig-fun &rest args)
   (message "newsticker--cache-update called, doing nothing"))

 (advice-add 'newsticker--cache-update :around #'newsticker--cache-update-advice)

 (add-hook 'newsticker-mode-hook #'my-newsticker-custom-keys)
 (add-hook 'newsticker-mode-hook #'hl-line-mode)
 (add-hook 'newsticker-select-feed-hook (lambda () (recenter-top-bottom 2))))


;;; nix-mode

(add-hook 'nix-mode-hook #'eglot-ensure)
(add-hook 'nix-mode-hook #'smartparens-mode)
;; (add-hook 'nix-mode-hook (lambda () (add-hook 'before-save-hook #'nix-format-buffer 0 t)))


;;; nov

(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(defun my-nov-update-text-width (n)
  "Change `nov-text-width' to width N."
  (interactive (list (or current-prefix-arg
                         (read-number (format "Change nov-text-width from %s to: " nov-text-width)
                                      (current-column)))))
  (setq-local nov-text-width n)
  (nov-render-document))

(defun my-nov-font-setup ()
  "`nov-mode' layout setup."
  (face-remap-add-relative 'default :height 1.1)
  (setq-local nov-text-width 80)
  (nov-render-document))

(add-hook 'nov-mode-hook #'my-nov-font-setup)
(keymap-set nov-mode-map "f" #'my-nov-update-text-width)


;;; orderless

(require 'orderless)
(setq completion-styles '(orderless basic)
      ;; Completion falls back to using completion-styles if
      ;; completion-category-overrides doesn't yield a
      ;; result
      completion-category-overrides '((file (styles . (basic flex partial-completion)))
                                      (buffer (styles . (basic flex partial-completion)))))


;;; org

(require 'org)
(require 'org-capture)

(defun org-refile-target-projects ()
  (directory-files "~/org/projects" t directory-files-no-dot-files-regexp))

(defun org-refile-target-howm-named-notes ()
  (directory-files howm-directory t ".*\\.org"))

(defun my-org-capture-file (path filename &optional template timestamp)
  "Return an org-mode file path of FILENAME stored under PATH within `org-directory'.

If FILENAME is the symbol 'title, prompt for a title to use and use it
as the org file's #+title, and then sanitize it and use it as the
filename.

If TIMESTAMP is a non-nil value, the resulting filename will be prefixed
with a timestamp.

If TEMPLATE is given, it will be treated as the filename of a template
file and the contents of this file will be used as the initial contents
of the new org-mode file."
  (let* ((prefix (if (not timestamp)
                     ""
                   (format-time-string "%Y%m%d%H%M%S-")))
         (template-file (if (not template)
                            ""
                          (expand-file-name
                           template
                           (concat user-emacs-directory "org-templates/"))))
         (dest-dir (concat org-directory "/" path))
         (title (if (eq filename 'title) (read-string "Title: ")))
         (name (if (eq filename 'title)
                   (format "%s.org" (my-sanitize-string title))
                 filename))
         (org-file (expand-file-name
                    (format "%s%s" prefix name)
                    dest-dir)))
    (when (and template (not (file-exists-p org-file)))
      ;; Template given and file does not exist; create it
      (unless (file-directory-p dest-dir)
        (make-directory dest-dir t))
      (with-temp-file org-file
        (insert
         ;; Let org-capture consume and fill out the template
         (org-capture-fill-template
          (with-temp-buffer
            (insert-file-contents template-file)
            (when (eq filename 'title)
              (goto-char (point-min))
              (while (re-search-forward "^\\(#\\+title: .*\\)%\\^{.*}\\(.*\\)$" nil t 1)
                (replace-match (format "\\1%s\\2" title))))
            (buffer-string))))))
    (abbreviate-file-name org-file)))

(setq org-adapt-indentation nil
      org-agenda-category-icon-alist '()
      org-agenda-files (expand-file-name "org-agenda-files" user-emacs-directory)
      org-agenda-search-view-max-outline-level 2
      org-agenda-start-with-follow-mode nil
      org-agenda-text-search-extra-files '(agenda-archives)
      org-archive-file-header-format "#+filetags: :ARCHIVE:\n\n"
      org-archive-location "archive/%s::"
      org-archive-reversed-order t
      org-attach-expert nil
      org-attach-id-dir "~/org/data/"        ; To allow viewing attachments even when archived
      org-attach-method 'mv
      org-attach-preferred-new-method 'dir
      org-attach-store-link-p 'attached
      org-auto-align-tags nil
      org-complete-tags-always-offer-all-agenda-tags t
      org-cycle-inline-images-display t
      org-default-notes-file "~/org/inbox.org"
      org-descriptive-links t
      org-edit-src-content-indentation 0
      org-export-with-sub-superscripts '{}
      org-fontify-done-headline nil
      org-fontify-todo-headline nil
      org-hide-emphasis-markers t
      org-hide-leading-stars nil
      org-image-actual-width 500
      org-indent-mode-turns-on-hiding-stars t
      org-log-into-drawer t
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path 'file
      org-reverse-note-order t
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-src-fontify-natively nil
      org-src-window-setup 'current-window
      org-startup-indented nil
      org-startup-with-inline-images t
      org-tags-column 0
      org-use-fast-todo-selection 'expert
      org-use-sub-superscripts '{}

      org-agenda-custom-commands
      '(("I" "Ideate TODOs" tags-todo "ideate")
        ("P" "All TODOs" ((tags-todo "@project-CATEGORY=\"routines\"") (tags-todo "@inbox")))
        ("R" "All routines" ((tags-todo "+CATEGORY=\"routines\""))))

      org-capture-templates '(("c" "Capture something" entry
                               (file org-default-notes-file)
                               "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:"
                               :empty-lines 1
                               :prepend t)

                              ("m" "Meeting")

                              ("mh" "Huddle" entry
                               (file (lambda ()
                                       (my-org-capture-file "meetings/"
                                                            "huddles.org"
                                                            "huddle-template.org")))
                               "* %T %?"
                               :empty-lines 1
                               :prepend t
                               :unnarrowed t)

                              ("mm" "Meeting" plain
                               (file (lambda ()
                                       (my-org-capture-file "meetings/"
                                                            'title
                                                            "meeting-template.org"
                                                            t)))
                               "%?"
                               :empty-lines 1
                               :unnarrowed t)

                              ("ms" "Standup/DSM" entry
                               (file (lambda ()
                                       (my-org-capture-file "meetings/"
                                                            "standups.org"
                                                            "standup-template.org")))
                               "* %T Standup\n\ndailystandup%?"
                               :empty-lines 1
                               :prepend t
                               :unnarrowed t)

                              ("M" "Meditation" entry (file "~/org/daily/meditation.org")
                               "* %^t\n%?"
                               :empty-lines 1
                               :prepend t)

                              ("t" "Topic" entry
                               (file org-default-notes-file)
                               "* TOPIC %? %^g\n:PROPERTIES:\n:CREATED:  %U\n:END:"
                               :empty-lines 1
                               :prepend t))

      org-refile-targets '((nil :maxlevel . 2)
                           ;; ("~/org/inbox.org" :todo . "TOPIC")
                           ;; (org-refile-target-projects :todo . "INBOX")
                           ;; (org-refile-target-projects :todo . "TODO")
                           (org-refile-target-howm-named-notes :maxlevel . 1))

      org-todo-keywords '((sequence "TODO(t)" "WIP(p)" "DEFERRED(f@)" "WAITING(w@)"
                                    "|" "DONE(d@/@)" "CANCELED(c@/@)")
                          (type "INBOX(i)" "TOPIC(o)"))

      org-todo-keyword-faces '(("CANCELED" :foreground "black" :background "gainsboro" :box (:style released-button) :height 0.8)
                               ("DEFERRED" :foreground "black" :background "azure1" :box (:style released-button) :height 0.8)
                               ("DONE" :foreground "black" :background "honeydew1" :box (:style released-button) :height 0.8)
                               ("INBOX" :foreground "black" :background "lightskyblue1" :box (:style released-button) :height 0.8)
                               ("TODO" :foreground "black" :background "mistyrose1" :box (:style released-button) :height 0.8)
                               ("TOPIC" :foreground "black" :background "slategray1" :box (:style released-button) :height 0.8)
                               ("WAITING" :foreground "black" :background "plum1" :box (:style released-button) :height 0.8)
                               ("WIP" :foreground "black" :background "peachpuff1" :box (:style released-button) :height 0.8)))

(with-eval-after-load 'org
  (require 'ox-asciidoc)
  (require 'ox-md)
  (require 'ox-gfm)
  (require 'ox-jira)
  (require 'ox-slack))

(defun org-fixup-whitespace ()
  "Fix Org document indentation, and tag alignment if universal arg was given."
  (interactive)
  (if (region-active-p)
      (org-indent-region (region-beginning) (region-end))
    (org-indent-region (point-min) (point-max)))
  ;; Call with a C-u prefix to fixup tag indentation
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-set-tags-command)))

(defun org-refile-to-topic ()
  "Limit `org-refile-targets' to those having todo keyword TOPIC in the current file plus the inbox."
  (interactive)
  (let ((org-refile-use-outline-path t)
        (org-refile-targets '((nil :todo . "TOPIC")
                              ("~/org/inbox.org" :todo . "TOPIC"))))
    (org-refile)))

(defun org-capture-refile-to-topic ()
  "Limit `org-refile-targets' to those having todo keyword TOPIC in the current file plus the inbox."
  (interactive)
  (let ((org-refile-use-outline-path t)
        (org-refile-targets '((nil :todo . "TOPIC")
                              ("~/org/inbox.org" :todo . "TOPIC"))))
    (org-capture-refile)))

(defun org-link-retain-description ()
  "Delete an org-mode-link and retain only the description."
  (interactive)
  (let* ((element (org-element-context))
         (element-type (car element)))
    (when (eq 'link element-type)
      (let* ((contents (buffer-substring (org-element-property :contents-begin element)
                                         (org-element-property :contents-end element))))
        (delete-region (org-element-property :begin element)
                       (org-element-property :end element))
        (insert contents)))))

(defun my-org-attach ()
  "Use Downloads as the default target directory."
  (interactive)
  (let ((dired-dwim-target (lambda ()
                             (list "~/Downloads/"))))
    (call-interactively #'org-attach)))

(defun my-org-mode-config ()
  "`org-mode' config."
  (keymap-set org-mode-map "C-c C-a" #'my-org-attach)
  (keymap-set org-mode-map "C-c C--" #'org-ctrl-c-minus)
  (keymap-set org-mode-map "C-c C-8" #'org-ctrl-c-star)
  (keymap-set org-mode-map "C-c C-SPC" #'org-table-blank-field)
  (keymap-set org-mode-map "C-c C-M-t" #'org-refile-to-topic)
  (keymap-set org-mode-map "C-M-q" #'org-fixup-whitespace)
  (keymap-set org-mode-map "C-M-h" #'org-mark-subtree)
  (keymap-set org-mode-map "C-c o L" #'org-link-retain-description)
  (keymap-set org-mode-map "C-c o >" #'my-calendar-mark-org-headings)
  (keymap-set org-capture-mode-map "C-c C-M-t" #'org-capture-refile-to-topic)
  (keymap-unset org-mode-map "C-,")
  ;; requires consult
  (keymap-set org-mode-map "C-c *" #'consult-org-heading))

(defun my-org-id-get-create ()
  "Put an ID in new nodes except for org-roam dailies."
  (unless (string-match-p "~/org/daily/"
                          (abbreviate-file-name
                           ;; While capturing, this returns the
                           ;; destination buffer, and while refiling
                           ;; since org-capture-get will be nil, the
                           ;; refile destination buffer is returned
                           ;; instead
                           (buffer-file-name (org-capture-get :buffer t))))
    (org-id-get-create)))

(defun my-add-org-property-dir (&rest _)
  "Add DIR property to the org element."
  (unless (org-entry-get (point) "DIR")
    (let* ((base-name (file-name-base (buffer-name)))
           (relative-dir (if (not (equal base-name "inbox"))
                             base-name
                           (org-back-to-heading-or-point-min)
                           (or (plist-get (cadr (org-element-at-point)) :raw-value)
                               "inbox_header"))))
      (org-set-property "DIR" (concat org-attach-id-dir
                                      (downcase (my-sanitize-string relative-dir)))))))
;; Covers both org-attach and org-attach-dired-to-subtree
(advice-add 'org-attach-attach :before #'my-add-org-property-dir)

(defun my-buffer-has-org-todo-p (buffer)
  "Returns t if org-mode BUFFER contains a todo header."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((todo-keywords-regexp (format "^\\*+ \\(%s\\)\\( \\|$\\) *"
                                          (mapconcat #'(lambda (elt)
                                                         (substring-no-properties elt))
                                                     org-not-done-keywords "\\|"))))
        (not (not (re-search-forward todo-keywords-regexp nil t)))))))

(defun my-org-update-org-agenda-files ()
  "Update `org-agenda-files' whenever a todo item is found in the buffer."
  (let ((current-buffer-entry (string-trim
                               (string-trim
                                (abbreviate-file-name (buffer-file-name))
                                org-directory)
                               "/"))
        (current-buffer (current-buffer)))
    (copy-file org-agenda-files (concat org-agenda-files ".bak") t)

    (with-temp-buffer
      (insert-file-contents org-agenda-files)
      (goto-char (point-min))
      (if (my-buffer-has-org-todo-p current-buffer)
          (insert (concat current-buffer-entry "\n"))
        (delete-matching-lines (format "^%s$" current-buffer-entry)))
      (sort-lines nil (point-min) (point-max))
      (delete-duplicate-lines (point-min) (point-max) nil t)
      (let ((inhibit-message t))
        (write-region (point-min) (point-max) org-agenda-files)))))

(defun my-org-update-org-agenda-on-save ()
  (add-hook 'before-save-hook #'my-org-update-org-agenda-files nil t))

(add-hook 'org-agenda-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'no-indent-tabs-mode)
(add-hook 'org-mode-hook #'my-org-mode-config)
;; (add-hook 'org-mode-hook #'my-org-update-org-agenda-on-save)
(add-hook 'org-mode-hook #'visual-line-fill-column-mode)
;; (add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-after-refile-insert-hook #'my-org-id-get-create)
(add-hook 'org-capture-prepare-finalize-hook #'beginning-of-buffer) ; so the ID property gets added to the root node
(add-hook 'org-capture-prepare-finalize-hook #'whitespace-cleanup)
(add-hook 'org-capture-prepare-finalize-hook #'single-blank-lines-only)
(add-hook 'org-capture-before-finalize-hook #'my-org-id-get-create)

(defun my-org-capture-inbox (goto)
  (interactive "P")
  (org-capture goto "c"))

(defun my-org-dired ()
  (interactive)
  (dired org-directory))

(defun my-org-inactive-timestamp (arg)
  "Insert org-mode inactive timestamp without %a (day of week)."
  (interactive "P")
  (let ((org-time-stamp-formats '("%Y-%m-%d" . "%Y-%m-%d %H:%M")))
    (org-time-stamp arg t)))

(keymap-global-set "C-c C" #'org-capture)
(keymap-global-set "C-c o a" #'org-agenda)
(keymap-global-set "C-c o b" #'org-switchb)
(keymap-global-set "C-c o s" #'org-store-link)
(keymap-global-set "C-c o v" #'visible-mode)


;;; org-agenda

(require 'org-agenda)
(setq org-agenda-show-outline-path nil)


;;; org-alert

(with-eval-after-load 'alert
  (require 'org-alert)
  (setq org-alert-interval 300
        org-alert-notification-title "Org alert"
        org-alert-notify-cutoff 0
        org-alert-notify-after-event-cutoff 10)
  (alert-add-rule :title org-alert-notification-title
                  :style 'libnotify
                  :continue t
                  :persistent t)
  (alert-add-rule :title org-alert-notification-title
                  :style 'org-alert-email
                  :continue t)
  (alert-add-rule :title org-alert-notification-title
                  :style 'legacy-log
                  :continue t)
  (org-alert-enable))


;;; org-babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((calc . t)
   (emacs-lisp . t)
   (js . t)
   (nix . t)
   (kotlin . t)
   (latex . t)
   (plantuml . t)
   (python . t)
   (shell . t)))

(require 'ob-chatgpt-shell)
(ob-chatgpt-shell-setup)


;;; org-bookmark-heading
(require 'org-bookmark-heading)


;;; org-crypt

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-crypt-key "0x837541986DF6E7AB"
      org-crypt-disable-auto-save t)
(add-to-list 'org-tags-exclude-from-inheritance "crypt")
(keymap-set org-mode-map "C-c o e" #'org-encrypt-entry)
(keymap-set org-mode-map "C-c o d" #'org-decrypt-entry)
(keymap-set org-mode-map "C-c o E" #'org-encrypt-entries)
(keymap-set org-mode-map "C-c o D" #'org-decrypt-entries)


;;; org-modern

(skipped
 (require 'org-modern)
 (setq org-modern-block-name nil
       org-modern-block-fringe nil
       org-modern-checkbox nil
       org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("▶" . "▼") ("▷" . "▽") ("▶" . "▼"))
       org-modern-hide-stars nil
       org-modern-keyword nil
       org-modern-list nil
       org-modern-priority t
       org-modern-priority-faces '((?A :background "whitesmoke" :foreground "hotpink" :weight bold :box (:style released-button :line-width (0 . -1)))
                                   (?B :background "whitesmoke" :foreground "cadetblue" :weight bold :box (:style released-button :line-width (0 . -1)))
                                   (?C :background "whitesmoke" :foreground "gray" :weight bold :box (:style released-button :line-width (0 . -1))))
       org-modern-progress nil
       org-modern-radio-target '("「" t "」")
       org-modern-replace-stars "■▪■▪■▪"
       org-modern-star nil
       org-modern-tag t
       org-modern-tag-faces '((t :background "beige" :foreground "black" :weight normal :box (:style pressed-button :line-width (0 . -1))))
       org-modern-timestamp nil
       org-modern-todo nil
       org-modern-todo-faces '(("CANCELED" :background "gainsboro" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                               ("DEFERRED" :background "azure1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                               ("DONE" :background "honeydew1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                               ("INBOX" :background "lightskyblue1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                               ("TODO" :background "mistyrose1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                               ("TOPIC" :background "slategray1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                               ("WAITING" :background "plum1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                               ("WIP" :background "peachpuff1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))))

 (defun my-org-modern-mode-toggle ()
   "Toggle `org-modern-mode' with some minor customization."
   (interactive)
   (cond ((not org-modern-mode) (progn (org-modern-mode)
                                       (setq line-spacing 0.1)))
         (t (progn (org-modern-mode -1)
                   (setq line-spacing nil)))))

 (keymap-set org-mode-map "C-c o m" #'my-org-modern-mode-toggle)
 (add-hook 'org-mode-hook #'my-org-modern-mode-toggle))


;;; org-present

(with-eval-after-load 'org-present
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-display-inline-images)
              (org-present-hide-cursor)
              (read-only-mode)))

  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (read-only-mode -1))))


;;; org-roam

(skipped
 (require 'org-roam)
 (defvar org-roam-content-width 60)

 (setq org-roam-directory "~/org"
       org-roam-db-location "~/org/org-roam.db"
       org-roam-completion-everywhere t
       org-roam-node-display-template (concat
                                       "${title:100} "
                                       (propertize "${tags}" 'foreground 'default)
                                       "${myarchive-itags}"
                                       "${mytodo}")
       org-roam-capture-templates '(("d" "default" plain "%?"
                                     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                     :empty-lines 1
                                     :unnarrowed t)

                                    ("l" "literature" plain "%?"
                                     :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                                                        "#+filetags: :@lit:\n#+title: ${title}")
                                     :empty-lines 1
                                     :unnarrowed t)

                                    ("x" "index" plain "%?"
                                     :target (file+head "index/%<%Y%m%d%H%M%S>-${slug}.org"
                                                        "#+filetags: :@lit:index:\n#+title: ${title}\n#+author: %^{author}")
                                     :empty-lines 1
                                     :unnarrowed t)

                                    ("r" "reference" plain "%?"
                                     :target (file+head "refs/%<%Y%m%d%H%M%S>-${slug}.org"
                                                        "#+filetags: :ref:\n#+title: ${title}")
                                     :empty-lines 1
                                     :unnarrowed t)

                                    ("I" "ideate" plain "%?"
                                     :target (file+head "ideate/%<%Y%m%d%H%M%S>-${slug}.org"
                                                        "#+filetags: :ideate:\n#+title: ${title}")
                                     :empty-lines 1
                                     :unnarrowed t))

       org-roam-dailies-capture-templates '(("d" "default" entry "* %?"
                                             :target (file+head "%<%Y-%m-%d>.org" "#+title: Journal - %<%Y-%m-%d>\n")
                                             :empty-lines 1)))

 (cl-defmethod org-roam-node-mytodo ((node org-roam-node))
   ;; Show the todo keywords when doing `org-roam-node-find'.
   (let ((todo (org-roam-node-todo node)))
     (when todo
       (format " #%s" (cond ((equal todo "DONE") (propertize todo 'face 'bold))
                            ((equal todo "CANCELED") (propertize todo 'face 'bold))
                            (t (propertize todo 'face 'bold)))))))

 (cl-defmethod org-roam-node-myarchive-itags ((node org-roam-node))
   ;; Show some tags for archived entries when doing `org-roam-node-find'.
   (let* ((archive-tags (cdr (assoc "ARCHIVE_ITAGS" (org-roam-node-properties node))))
          (dropped-tags '("@inbox" "@archive" "INBOX" "ARCHIVE")))
     (when archive-tags
       (let ((tags))
         (mapcar (lambda (elt)
                   (unless (member elt dropped-tags) (push (concat "#" elt) tags)))
                 (string-split archive-tags " "))
         (format " %s" (propertize (string-join tags " ") 'foreground 'default))))))

 (add-to-list 'display-buffer-alist
              `("\\*org-roam\\*"
                (display-buffer-in-direction)
                (direction . left)
                (window-width . ,(+ 5 org-roam-content-width))
                (window-height . fit-window-to-buffer)))

 (org-roam-db-autosync-mode)

 (defun my-org-subtree-root-tags ()
   "Return the tags of the root header of the current subtree."
   (save-excursion
     (org-back-to-heading t)
     (while (org-up-heading-safe))
     (org-get-tags nil t)))

 (defun my-org-roam-extract-subtree-inbox-entry ()
   "Use the tag in the root topic node as the destination directory within `org-roam-directory'."
   (interactive)
   (let* ((root-tags (my-org-subtree-root-tags))
          (subdir (string-replace "@" "" (car root-tags)))
          (subdir-sanitized (if subdir (my-sanitize-string subdir) nil))
          (relative-path (if subdir-sanitized (concat "/projects/" subdir-sanitized "/") ""))
          (org-roam-directory (concat org-roam-directory relative-path)))
     ;; Apply the root's tags to the extracted child
     (save-excursion
       (org-back-to-heading)
       (org-set-tags (append (org-get-tags nil t) root-tags)))
     (org-roam-extract-subtree)))

 (defun org-roam-node-insert-immediate-finish ()
   (interactive)
   (let ((org-roam-capture-templates (mapcar (lambda (elt)
                                               (append elt '(:immediate-finish t)))
                                             org-roam-capture-templates)))
     (call-interactively #'org-roam-node-insert)))

 (defun my-org-roam-add-link-to-region ()
   "Link the selected region to an org-roam node."
   (interactive)
   (add-hook 'minibuffer-setup-hook
             #'(lambda () (when (minibufferp) (delete-minibuffer-contents))))
   (org-roam-node-insert)
   (remove-hook 'minibuffer-setup-hook
                #'(lambda () (when (minibufferp) (delete-minibuffer-contents)))))

 (cl-defun my-org-roam-capture (&optional goto keys &key filter-fn templates info)
   (interactive "P")
   (org-roam-capture- :goto goto
                      :info info
                      :keys keys
                      :templates templates
                      :node (org-roam-node-create :title "notitle")
                      :props '(:immediate-finish nil)))


 (add-hook 'org-roam-mode-hook (lambda ()
                                 (visual-line-fill-column-mode)
                                 (set-fill-column org-roam-content-width)))

 (keymap-global-set "C-c n %" #'org-roam-node-random)
 (keymap-global-set "C-c n ," #'org-roam-dailies-goto-previous-note)
 (keymap-global-set "C-c n ." #'org-roam-dailies-goto-next-note)
 (keymap-global-set "C-c n C" #'org-roam-capture)
 (keymap-global-set "C-c n J" #'org-roam-dailies-capture-date)
 (keymap-global-set "C-c n c" #'my-org-roam-capture)
 (keymap-global-set "C-c n f" #'org-roam-node-find)
 (keymap-global-set "C-c n j" #'org-roam-dailies-capture-today)
 (keymap-global-set "C-c n y" #'org-roam-dailies-goto-yesterday)
 (keymap-set org-mode-map "C-c n A" #'org-roam-alias-remove)
 (keymap-set org-mode-map "C-c n I" #'org-roam-node-insert-immediate-finish)
 (keymap-set org-mode-map "C-c n R" #'org-roam-ref-remove)
 (keymap-set org-mode-map "C-c n T" #'org-roam-tag-remove)
 (keymap-set org-mode-map "C-c n X" #'my-org-roam-extract-subtree-inbox-entry)
 (keymap-set org-mode-map "C-c n a" #'org-roam-alias-add)
 (keymap-set org-mode-map "C-c n b" #'org-roam-buffer-toggle)
 (keymap-set org-mode-map "C-c n i" #'org-roam-node-insert)
 (keymap-set org-mode-map "C-c n l" #'my-org-roam-add-link-to-region)
 (keymap-set org-mode-map "C-c n n" #'org-id-get-create)
 (keymap-set org-mode-map "C-c n r" #'org-roam-ref-add)
 (keymap-set org-mode-map "C-c n t" #'org-roam-tag-add)
 (keymap-set org-mode-map "C-c n x" #'org-roam-extract-subtree)
 (keymap-set org-roam-mode-map "C-c n l" #'org-roam-buffer-toggle))


;;; org-sticky-header

(setq org-sticky-header-at-point t
      org-sticky-header-heading-star "*"
      org-sticky-header-full-path 'full)


;;; org-web-tools

(keymap-set org-mode-map "C-c o C-y" #'org-web-tools-insert-link-for-url)
(keymap-set org-mode-map "C-c o w i" #'org-web-tools-insert-web-page-as-entry)
(keymap-set org-mode-map "C-c o w a" #'org-web-tools-archive-attach)
(keymap-set org-mode-map "C-c o w v" #'org-web-tools-archive-view)


;;; orgalist

(require 'orgalist)
(defun orgalist-insert-checkbox ()
  (interactive)
  (orgalist-insert-item t))
(defconst orgalist--maybe-insert-checkbox
  '(menu-item "" orgalist-insert-checkbox :filter orgalist--when-in-item))
(keymap-set orgalist-mode-map "M-<up>" orgalist--maybe-move-up)
(keymap-set orgalist-mode-map "M-<down>" orgalist--maybe-move-down)
(keymap-set orgalist-mode-map "C-c C--" orgalist--maybe-cycle-bullet)
(keymap-set orgalist-mode-map "M-S-<return>" orgalist--maybe-insert-checkbox)
(keymap-unset orgalist-mode-map "C-c -")

(defun my-orgalist-mode-config ()
  (when (or (eq major-mode 'text-mode)
            (and (buffer-file-name)
                 (string-equal (file-name-extension (buffer-file-name)) "txt")))
    (orgalist-mode 1)))

(add-hook 'text-mode-hook #'my-orgalist-mode-config)


;;; origami

(defun my-origami-mode-config ()
  "`origami-mode' config."
  (keymap-set origami-mode-map "C-c f f" #'origami-toggle-node)
  (keymap-set origami-mode-map "C-c f O" #'origami-open-all-nodes)
  (keymap-set origami-mode-map "C-c f C" #'origami-close-all-nodes))
(add-hook 'origami-mode-hook #'my-origami-mode-config)

;; Calling (global-origami-mode) directly causes issues on emacs --fg-daemon
(add-hook 'after-init-hook #'global-origami-mode)


;;; outline

(require 'outline)
(setq outline-default-state nil
      outline-minor-mode-cycle t)

(defun my-outline-ensure-space-after-heading ()
  (unless (char-equal ?\s (char-before))
    (insert " ")))
(add-hook 'outline-insert-heading-hook #'my-outline-ensure-space-after-heading)

(defun my-outline-convert-to-heading ()
  "Convert current line to a heading."
  (interactive)
  (save-excursion
    (cond ((outline-on-heading-p)
           (goto-char (pos-bol))
           (re-search-forward "^\\*+ " nil t)
           (replace-match ""))
          (t (kill-region (pos-bol) (pos-eol))
             (condition-case err
                 (outline-insert-heading)
               (error (insert "* ")))
             (yank)
             ;; handle case where only a space is inserted by outline-insert-heading
             ;; (happens when there are no other headings in the buffer)
             (goto-char (pos-bol))
             (when (not (char-equal (following-char) ?*))
               (insert "*"))))))

(defun my-outline-mode-config ()
  (keymap-set outline-mode-map "C-c"
              (let ((map (make-sparse-keymap)))
                (keymap-set map "@" #'outline-mark-subtree)
                (keymap-set map "C-8" #'my-outline-convert-to-heading)
                (keymap-set map "C-n" #'outline-next-visible-heading)
                (keymap-set map "C-p" #'outline-previous-visible-heading)
                (keymap-set map "C-u" #'outline-up-heading)
                (keymap-set map "C-f" #'outline-forward-same-level)
                (keymap-set map "C-b" #'outline-backward-same-level)
                (keymap-set map "C-o" #'outline-hide-other)
                map))
  (keymap-set outline-mode-map "M-<up>" #'outline-move-subtree-up)
  (keymap-set outline-mode-map "M-<down>" #'outline-move-subtree-down)
  (keymap-set outline-mode-map "M-<left>" #'outline-promote)
  (keymap-set outline-mode-map "M-<right>" #'outline-demote)
  (keymap-set outline-mode-map "C-<return>" #'outline-insert-heading))

(defun my-outline-minor-mode-config ()
  (keymap-set outline-minor-mode-map "C-c"
              (let ((map (make-sparse-keymap)))
                (keymap-set map "@" #'outline-mark-subtree)
                (keymap-set map "C-8" #'my-outline-convert-to-heading)
                (keymap-set map "C-b" #'outline-backward-same-level)
                (keymap-set map "C-f" #'outline-forward-same-level)
                (keymap-set map "C-n" #'outline-next-visible-heading)
                (keymap-set map "C-o" #'outline-hide-other)
                (keymap-set map "C-p" #'outline-previous-visible-heading)
                (keymap-set map "C-u" #'outline-up-heading)
                map))
  (keymap-set outline-minor-mode-map "M-<up>" #'outline-move-subtree-up)
  (keymap-set outline-minor-mode-map "M-<down>" #'outline-move-subtree-down)
  (keymap-set outline-minor-mode-map "M-<left>" #'outline-promote)
  (keymap-set outline-minor-mode-map "M-<right>" #'outline-demote)
  (keymap-set outline-minor-mode-map "C-<return>" #'outline-insert-heading))

(add-hook 'outline-mode-hook #'my-outline-mode-config)
;; (add-hook 'outline-minor-mode-hook #'my-outline-minor-mode-config)


;;; pdf-tools

(add-hook 'after-init-hook #'pdf-tools-install)


;;; personal-theme

(require 'personal-theme)
(enable-theme 'personal)


;;; plantuml

(require 'plantuml-mode)
(setq plantuml-default-exec-mode 'executable
      plantuml-jar-path "~/.nix-profile/bin/plantuml"
      plantuml-output-type "png")
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))


;;; prettier
(add-hook 'typescript-ts-mode-hook #'prettier-js-mode)


;;; prism

(skipped
 (add-hook 'lisp-mode-hook #'prism-mode)
 (add-hook 'emacs-lisp-mode-hook #'prism-mode)
 (add-hook 'clojure-mode-hook #'prism-mode)
 (add-hook 'clojurescript-mode-hook #'prism-mode)
 (add-hook 'scheme-mode-hook #'prism-mode)
 (add-hook 'python-mode-hook #'prism-whitespace-mode)
 (add-hook 'yaml-mode-hook #'prism-whitespace-mode)
 (keymap-global-set "C-c h b" #'prism-mode))


;;; prodigy

(require 'prodigy)
(setq prodigy-services '((:name "proton-bridge"
                                :command "protonmail-bridge"
                                :args ("-n")
                                :tags (proton))
                         (:name "sample-project"
                                :command "~/prj/sample/local_build.sh"
                                :args ("")
                                :cwd "~/prj/sample"
                                :tags (ctl-c)))
      prodigy-tags '((:name proton
                            :stop-signal int
                            :kill-process-buffer-on-stop t)
                     (:name ctl-c
                            :stop-signal int
                            :ready-message "Press CTRL\\+C to quit")))
(keymap-global-set "C-c P p" #'prodigy)


;;; project

;; Add magit
(keymap-set project-prefix-map "m" #'magit-project-status)
(add-to-list 'project-switch-commands '(magit-project-status "Magit") t)

;; Add deadgrep
(keymap-set project-prefix-map "G" #'deadgrep)
(add-to-list 'project-switch-commands '(deadgrep "Deadgrep") t)


;;; pytest

(with-eval-after-load 'python
  (require 'pytest)

  (defun pytest-cov-one ()
    (interactive)
    (let ((pytest-global-name "coverage run -m pytest"))
      (pytest-one)))

  (defun pytest-cov-again ()
    (interactive)
    (let ((pytest-global-name "coverage run -m pytest"))
      (pytest-again)))

  (defun pytest-cov-module ()
    (interactive)
    (let ((pytest-global-name "coverage run -m pytest"))
      (pytest-module)))

  (defun pytest-cov-all ()
    (interactive)
    (let ((pytest-global-name "coverage run -m pytest"))
      (pytest-all)))

  (keymap-set python-mode-map "C-c P ." #'pytest-one)
  (keymap-set python-mode-map "C-c P !" #'pytest-again)
  (keymap-set python-mode-map "C-c P m" #'pytest-module)
  (keymap-set python-mode-map "C-c P a" #'pytest-all)
  (keymap-set python-mode-map "C-c C ." #'pytest-cov-one)
  (keymap-set python-mode-map "C-c C !" #'pytest-cov-again)
  (keymap-set python-mode-map "C-c C m" #'pytest-cov-module)
  (keymap-set python-mode-map "C-c C a" #'pytest-cov-all))


;;; python

(setq python-indent-def-block-scale 1)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'blacken-mode)
(add-hook 'python-mode-hook #'subword-mode)

(defun python-pyright-make-config (selection)
  "Write pyrightconfig.json venv settings for a project."
  (interactive (list (read-directory-name "Which venv to use: "
                                          "~/.virtualenvs/"
                                          "~/.virtualenvs/"
                                          t)))
  (let ((venv-dir "~/.virtualenvs")
        (venv-name (car (last (split-string
                               (expand-file-name selection)
                               "/"
                               t))))
        (out-dir (vc-root-dir))
        (out-filename "pyrightconfig.json"))
    (if (not out-dir)
        (message "Cannot find project root")
      (with-temp-buffer
        (insert (json-encode `(:venvPath ,venv-dir :venv ,venv-name)))
        (json-pretty-print-buffer)
        (write-file (concat out-dir out-filename))
        (message "Wrote config to %s" (concat out-dir out-filename))))))

(keymap-set python-mode-map "C-c C-b" (lambda ()
                                        "Create a buffer-dedicated python shell."
                                        (interactive)
                                        (run-python (python-shell-calculate-command) t t)))
(keymap-set python-mode-map "C-c C-t" #'python-shell-restart)


;;; recentf

(require 'recentf)
(setq recentf-auto-cleanup 120
      recentf-max-menu-items 5000
      recentf-max-saved-items 10000
      recentf-menu-filter 'recentf-show-basenames-ascending)
(add-to-list 'recentf-exclude "\\/sudoedit:root")
(run-at-time nil (* 10 60) 'recentf-save-list) ; Save every 10 minutes

(defun recentf-cleanup-quietly (orig-fun &rest _)
  "Don't print log messages while calling ORIG-FUN."
  (let ((inhibit-message t)) (funcall orig-fun)))
(advice-add #'recentf-cleanup :around #'recentf-cleanup-quietly)

(defun recentf-save-list-quietly (orig-fun &rest _)
  "Don't print log messages while calling ORIG-FUN."
  (let ((inhibit-message t)) (funcall orig-fun)))
(advice-add #'recentf-save-list :around #'recentf-save-list-quietly)

(defun recentf-open-files-completing-read ()
  (interactive)
  (let* ((recentf-menu (recentf-make-menu-items))
         (menu-labels (mapcar (lambda (elt) (aref elt 0))
                              recentf-menu))
         (result (completing-read "Open recent: " menu-labels))
         (selection-pos (cl-position result menu-labels :test 'equal))
         (recentf-form (aref (nth selection-pos recentf-menu) 1)))
    (cond ((functionp recentf-form)
           ;; recentf uses lambdas to find-file an item
           (funcall recentf-form))
          ((eq 'customize-group (car recentf-form))
           ;; The item "More..." needs to be eval'd
           (eval recentf-form)))))

(keymap-global-set "C-x C-M-f" #'recentf-open-files-completing-read)
(add-hook 'after-init-hook #'recentf-mode)


;;; restclient

(require 'restclient)

(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

(defun my-restclient-config ()
  "`restclient-mode' config."
  (setq tab-width 2
        restclient-same-buffer-response nil
        restclient-response-size-threshold nil)
  (smartparens-strict-mode 1)
  (indent-tabs-mode -1))

(add-hook 'restclient-mode-hook #'my-restclient-config)

;; (defun restclient-no-gc-before-request ()
;;   (increase-gc-cons-percentage))
;; (defun restclient-gc-after-response ()
;;   (revert-gc-cons-percentage)
;;   (garbage-collect))
;; (defun restclient-format-response ()
;;   (show-paren-local-mode -1)
;;   (font-lock-mode -1)
;;   (buffer-disable-undo)
;;   (buffer-enable-undo))
;; (add-hook 'restclient-http-do-hook #'restclient-no-gc-before-request)
;; (add-hook 'restclient-response-loaded-hook #'restclient-gc-after-response)
;; (add-hook 'restclient-response-loaded-hook #'restclient-format-response)


;;; savehist

(savehist-mode)


;;; saveplace-pdf-view

(require 'saveplace-pdf-view)


;;; scheme

(defun my-scheme-mode-config ()
  (keymap-set scheme-mode-map "M-RET" #'scheme-send-last-sexp)
  (keymap-set scheme-mode-map "C-c m s" #'run-scheme))
(add-hook 'scheme-mode-hook #'my-scheme-mode-config)


;;; selected

(require 'selected)
(defun flush-empy-lines ()
  (interactive)
  (flush-lines "^$"))

(defun replace-region-with (string)
  "Replace text in the region with STRING."
  (interactive "sReplace region with: ")
  (save-excursion
    (let ((begin (use-region-beginning))
          (end (use-region-end)))
      (unless (or (null begin) (null end))
        (goto-char begin)
        (atomic-change-group ;; Treat the following as a single undo
          (while (< (point) end)
            (let ((line-start (if (< (pos-bol) begin) begin (pos-bol)))
                  (line-end (if (> (pos-eol) end) end (pos-eol))))
              (delete-region line-start line-end)
              (insert (let* ((len (- line-end line-start))
                             (string (mapconcat #'identity (make-list len string))))
                        (if (length> string len)
                            (substring string 0 len)
                          string)))
              (forward-line 1))))))))

(keymap-set selected-keymap "C" #'capitalize-region)
(keymap-set selected-keymap "E" #'flush-empy-lines)
(keymap-set selected-keymap "F" #'flush-lines)
(keymap-set selected-keymap "K" #'keep-lines)
(keymap-set selected-keymap "SPC" #'canonically-space-region)
(keymap-set selected-keymap "a" #'align-regexp)
(keymap-set selected-keymap "f" #'fill-region)
(keymap-set selected-keymap "r" #'reverse-region)
(keymap-set selected-keymap "s" #'sort-lines)
(keymap-set selected-keymap "S" #'my-sort-strings)
(keymap-set selected-keymap "u" #'unfill-region)
(keymap-set selected-keymap "q" #'selected-off)
(keymap-set selected-keymap "R" #'replace-region-with)
(selected-global-mode 1)

(defun turn-off-selected-minor-mode ()
  (selected-minor-mode -1))

(defun my-mc-toggle-selected-minor-mode ()
  (if multiple-cursors-mode
      (selected-minor-mode -1)
    (selected-minor-mode 1)))
(with-eval-after-load 'chatgpt-shell
  (add-hook 'chatgpt-shell-prompt-compose-mode-hook #'turn-off-selected-minor-mode))
(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook #'turn-off-selected-minor-mode))
(with-eval-after-load 'multiple-cursors
  (add-hook 'multiple-cursors-mode-hook #'my-mc-toggle-selected-minor-mode))


;;; slime

(setq inferior-lisp-program "clisp")


;;; smartparens

(require 'smartparens-config)

(setq sp-highlight-pair-overlay nil
      sp-show-pair-delay 0)

(defun my-smartparens-config ()
  (let ((map smartparens-mode-map))
    ;; movement
    (keymap-set map "C-M-b" #'sp-backward-sexp)
    (keymap-set map "C-M-d" #'sp-down-sexp)
    (keymap-set map "C-M-f" #'sp-forward-sexp)
    (keymap-set map "C-M-n" #'sp-next-sexp)
    (keymap-set map "C-M-p" #'sp-previous-sexp)
    (keymap-set map "C-M-u" #'sp-backward-up-sexp)
    ;; barf/slurp
    (keymap-set map "C-(" #'sp-backward-slurp-sexp)
    (keymap-set map "C-)" (if (memq major-mode sp-lisp-modes) #'sp-forward-slurp-sexp #'sp-slurp-hybrid-sexp))
    (keymap-set map "M-(" #'sp-backward-barf-sexp)
    (keymap-set map "M-)" #'sp-forward-barf-sexp)
    ;; editing
    (keymap-set map "M-C" #'sp-change-enclosing)
    (keymap-set map "M-D" #'sp-kill-symbol)
    (keymap-set map "M-J" #'sp-join-sexp)
    (keymap-set map "M-R" #'sp-raise-sexp)
    (keymap-set map "M-S" #'sp-splice-sexp)
    (keymap-set map "M-U" #'sp-unwrap-sexp)
    (keymap-set map "M-W" #'sp-wrap-round)
    (keymap-set map "C-M-<backspace>" #'sp-backward-kill-sexp)
    (keymap-set map "M-S-<backspace>" #'sp-backward-kill-symbol)
    (keymap-set map "C-c s C" #'sp-convolute-sexp)
    nil))

(defun setup-smartparens-lisp ()
  (smartparens-mode)
  (smartparens-strict-mode))

(add-hook 'clojure-mode-hook #'setup-smartparens-lisp)
(add-hook 'emacs-lisp-mode-hook #'setup-smartparens-lisp)
(add-hook 'eval-expression-minibuffer-setup-hook #'setup-smartparens-lisp)
(add-hook 'lisp-mode-hook #'setup-smartparens-lisp)
(add-hook 'scheme-mode-hook #'setup-smartparens-lisp)

(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'howm-mode-hook #'smartparens-mode)
(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'kotlin-mode-hook #'smartparens-mode)
(add-hook 'latex-mode-hook #'smartparens-mode)
(add-hook 'org-mode-hook #'smartparens-mode)
(add-hook 'prog-mode-hook #'smartparens-mode)

(add-hook 'json-mode-hook #'smartparens-strict-mode)
(add-hook 'python-mode-hook #'smartparens-strict-mode)

(add-hook 'smartparens-mode-hook #'my-smartparens-config)


;;; solidity-mode

(require 'solidity-mode)


;;; tide

(require 'tide)
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)


;;; time

(require 'time)
(setq display-time-24hr-format t
      display-time-default-load-average 0
      display-time-day-and-date nil)
(display-time-mode t)


;;; transpose-frame

(require 'transpose-frame)
(keymap-global-set "C-c t f" #'transpose-frame)


;;; treesit

;; Language grammar downloads:
;; https://github.com/emacs-tree-sitter/tree-sitter-langs
(require 'treesit)
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (mermaid "https://github.com/monaqa/tree-sitter-mermaid")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")))


;;; typescript-ts-mode

(require 'typescript-ts-mode)
(setq typescript-ts-mode-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
(add-hook 'typescript-ts-mode-hook #'smartparens-mode)
(add-hook 'typescript-ts-mode-hook #'smartparens-strict-mode)
(add-hook 'typescript-ts-mode-hook #'show-smartparens-mode)
(add-hook 'typescript-ts-mode-hook #'subword-mode)


;;; visual-wrap
(global-visual-wrap-prefix-mode 1)


;;; which-key

(skipped
 (require 'which-key)
 (setq which-key-idle-delay 1
       which-key-lighter nil)
 (add-hook 'after-init-hook #'which-key-mode))


;;; winfast

(require 'winfast)
(keymap-global-set "M-`" #'winfast-mode)


;;; writeroom-mode

(setq writeroom-extra-line-spacing 0.2
      writeroom-fringes-outside-margins t
      writeroom-global-effects '(writeroom-set-alpha
                                 writeroom-set-menu-bar-lines
                                 writeroom-set-tool-bar-lines
                                 writeroom-set-vertical-scroll-bars
                                 writeroom-set-bottom-divider-width)
      writeroom-maximize-window nil
      writeroom-width 100)


;;; xref

(defun my-xref-config ()
  (interactive)
  (keymap-set xref--xref-buffer-mode-map "b" #'outline-backward-same-level)
  (keymap-set xref--xref-buffer-mode-map "f" #'outline-forward-same-level))
(add-hook 'xref--xref-buffer-mode-hook #'my-xref-config)
(add-hook 'xref--xref-buffer-mode-hook #'outline-minor-mode)


;;; xref-js2

(with-eval-after-load 'js2-mode
  (defun my-xref-js2-config ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend))
  (add-hook 'js2-mode-hook #'my-xref-js2-config))


;;; yasnippet

(require 'yasnippet)
(setq yas-minor-mode-map (let ((map (make-sparse-keymap)))
                           (keymap-set map "s" 'yas-expand)
                           (keymap-set map "i" 'yas-insert-snippet)
                           (keymap-set map "n" 'yas-new-snippet)
                           (keymap-set map "v" 'yas-visit-snippet-file)
                           (keymap-set map "t" 'yas-tryout-snippet)
                           (keymap-set map "d" 'yas-describe-tables)
                           (keymap-set map "r" 'yas-reload-all)
                           map)
      yas-prompt-functions '(yas-completing-prompt yas-no-prompt)
      yas-triggers-in-field t
      yas-wrap-around-region t)

(keymap-global-set "C-c y s" yas-minor-mode-map)
(yas-global-mode t)


(provide 'init)
;;; init.el ends here
