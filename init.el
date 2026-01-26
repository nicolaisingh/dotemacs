;;; init.el --- Nicolai Singh's .emacs -*- lexical-binding: t -*-

;;; Commentary:

;; This contains my personal Emacs settings.

;;; Code:

;;;; Startup

(defmacro skipped (&rest body)
  "Discard BODY."
  (declare (indent nil))
  nil)

;; Avoid garbage collection during startup
(setq original-gc-cons-percentage gc-cons-percentage
      higher-gc-cons-percentage 0.5)

(defun increase-gc-cons-percentage ()
  "Set a higher value for gc-cons-percentage to prevent garbage
collection.  Use revert-gc-cons-percentage to restore the value."
  (setq gc-cons-percentage higher-gc-cons-percentage))

(defun revert-gc-cons-percentage ()
  "Restore gc-cons-percentage to its original value."
  (setq gc-cons-percentage original-gc-cons-percentage))

(increase-gc-cons-percentage)
(add-hook 'after-init-hook (lambda ()
                             (revert-gc-cons-percentage)
                             (garbage-collect)))

;; Garbage collect during idle time
(setq gc-idle-timer nil)
(defun toggle-gc-idle-timer ()
  "Set a timer that GCs when Emacs idles, like package gcmh."
  (when (timerp gc-idle-timer) (cancel-timer gc-idle-timer))
  (setq gc-idle-timer (run-with-idle-timer 5 t #'garbage-collect)))
(toggle-gc-idle-timer)

;; Report startup time
(defun message-init-time ()
  "Report Emacs init time."
  (message "Emacs init time: %s with %d GCs" (emacs-init-time) gcs-done))
(add-hook 'emacs-startup-hook #'message-init-time)
;; (add-hook 'after-init-hook #'server-start)

;; Manually load early-init.el for older versions of Emacs
(when (< emacs-major-version 27)
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (when (file-exists-p early-init-file)
      (load early-init-file))))

;; Customize use-package
(setopt use-package-always-defer t
        use-package-always-ensure t
        use-package-compute-statistics t
        use-package-hook-name-suffix nil
        use-package-verbose nil)

(defvar my-music-playlist-directory "~/Music/playlists/")
(defvar my-music-library-directory "~/Music/library/")
(defvar my-music-flac-directory "~/Music/flac/")

;; Load package manager elpaca
(load (expand-file-name "lisp/elpaca/install.el" user-emacs-directory))
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(keymap-global-set "C-h u m" #'elpaca-manager)
(keymap-global-set "C-h u l" #'elpaca-log)
(keymap-global-set "C-h u v" #'elpaca-visit)
(setopt elpaca-lock-file
        (expand-file-name "lisp/elpaca/lockfile.eld" user-emacs-directory))

(defun my-elpaca-write-lock-file ()
  "Write the elpaca lock file to my preferred location."
  (interactive)
  (elpaca-write-lock-file elpaca-lock-file))

;; Include the ff. directories when searching for files to load
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Keybinding prefixes
(define-prefix-command 'my-ctl-c-D-map)
(define-prefix-command 'my-ctl-c-M-map)
(define-prefix-command 'my-ctl-c-P-map)
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
(define-prefix-command 'my-meta-=-map)
(define-prefix-command 'my-meta-o-map)
(keymap-global-set "C-c D" 'my-ctl-c-D-map)
(keymap-global-set "C-c M" 'my-ctl-c-M-map)
(keymap-global-set "C-c P" 'my-ctl-c-P-map)
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
(keymap-global-set "M-=" 'my-meta-=-map)
(keymap-global-set "M-o" 'my-meta-o-map)


;;;; Functions

(defun my-sort-init-file ()
  "Sort the init file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^;;;; Package loads$")
    (let ((start (point)))
      (point-marker)
      (end-of-buffer)
      (backward-page)
      (sort-pages nil start (point)))))

(defmacro define-set-indent-tab-width-fn (width)
  "Macro to define a simple function to set `tab-width' to WIDTH."
  `(defun ,(intern (format "set-indent-tab-width-%d" width)) ()
     (interactive)
     ,(format "Set `tab-width' to %d." width)
     (setq tab-width ,width)))
(define-set-indent-tab-width-fn 2)
(define-set-indent-tab-width-fn 4)
(define-set-indent-tab-width-fn 8)

(defun no-indent-tabs-mode ()
  "Turn of `indent-tabs-mode' (i.e. indent using spaces)."
  (indent-tabs-mode -1))

(defun my-select-tab-or-other-window (arg)
  (interactive "P")
  (cond
   ((eq 0 arg) (tab-recent))
   ((eq '- arg) (window-swap-states))
   ((equal '(4) arg) (delete-other-windows))
   ((equal '(16) arg) (delete-other-windows-vertically))
   ((numberp arg) (tab-bar-select-tab arg))
   (t (if (> (count-windows) 1)
          (other-window 1)
        (split-window-right)
        (other-window 1)))))

(defun find-init-file ()
  "Find my Emacs init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun find-scratch-buffer ()
  "Find the *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun indent-using-tabs-and-fixup ()
  (interactive)
  (indent-tabs-mode 1)
  (tabify (point-min) (point-max)))

(defun indent-using-spaces-and-fixup ()
  (interactive)
  (indent-tabs-mode -1)
  (untabify (point-min) (point-max)))

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

(defun sudo-find-alternate-file ()
  (interactive)
  (require 'tramp)
  (find-alternate-file (concat "/sudoedit::" (buffer-file-name))))

(defun file-split-buffer-by-lines (lines-per-file header-line)
  "Split buffer into multiple files with LINES-PER-FILE each having HEADER-LINE on top."
  (interactive (list (read-number "Lines per file: ")
                     (read-string "Header per file (empty for none): ")))
  (let ((part 1)
        (file-base-name (file-name-sans-extension (buffer-name)))
        (extension (file-name-extension (buffer-name)))
        (line-start 1))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((file-part-name (format "%s.part-%d.%s" file-base-name part extension))
              (orig-buffer (current-buffer))
              (start)
              (end))
          (setq start (point))
          (forward-line lines-per-file)
          (setq end (point))
          (with-temp-buffer
            (when (and header-line (not (string-empty-p header-line)))
              (insert header-line "\n"))
            (insert-buffer-substring orig-buffer start end)
            (write-region (point-min) (point-max) file-part-name)))
        (setq part (1+ part))))))

(defun unhighlight-all-regexp ()
  (interactive)
  (mapc (lambda (elt)
          (unhighlight-regexp (car elt)))
        hi-lock-interactive-lighters))

(defun delete-leading-whitespace (&optional start end)
  "Delete leading whitespace between START and END.
If called interactively, START and END are the start/end of the
region if the mark is active, or of the buffer's accessible
portion if the mark is inactive."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (save-match-data
    (save-excursion
      (let ((end-marker (and end (copy-marker end))))
        (goto-char (or start (point-min)))
        (with-syntax-table (make-syntax-table (syntax-table))
          ;; Don't delete formfeeds, even if they are considered whitespace.
          (modify-syntax-entry ?\f "_")
          (while (re-search-forward "^\\s-" end-marker t)
            (skip-syntax-forward "-" (line-end-position))
            (let ((b (point)) (e (line-beginning-position)))
              (if (region-modifiable-p b e)
                  (delete-region b e)
                (goto-char e)))))
        (if end
            (set-marker end-marker nil)
          ;; Delete trailing empty lines.
          (and  delete-trailing-lines
                ;; Really the end of buffer.
                (= (goto-char (point-max)) (1+ (buffer-size)))
                (<= (skip-chars-backward "\n") -2)
                (region-modifiable-p (1+ (point)) (point-max))
                (delete-region (1+ (point)) (point-max)))))))
  ;; Return nil for the benefit of `write-file-functions'.
  nil)

(defun completion-metadata-get-category ()
  "Return the current completion category."
  (interactive)
  (message "Completion category: %s"
           (completion-metadata-get
            (completion-metadata "" minibuffer-completion-table minibuffer-completion-predicate)
            'category)))


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


;;;; Package loads


;;; 00 emacs

(use-package emacs
  :demand t
  :ensure nil ; built-in package
  :bind (([remap eval-last-sexp] . pp-eval-last-sexp)
         ("C-`" . my-select-tab-or-other-window)
         ("C-c j" . (lambda (jira-issue)
                      (interactive
                       (list (let ((initial-input (symbol-name (or (symbol-at-point)
                                                                   (intern "")))))
                               (completing-read "Jira issue: " nil nil nil initial-input))))
                      (browse-url (format "https://JIRA.atlassian.net/browse/%s" jira-issue))))
         ("C-h C-c" . find-function-on-key)
         ("C-h C-f" . find-function)
         ("C-h C-k" . describe-keymap)
         ("C-h C-v" . find-variable)
         ("C-h u f" . find-library)
         ("C-x B" . bury-buffer)
         ("C-x C-M-c" . save-buffers-kill-emacs)
         ("C-x D" . (lambda () (interactive) (dired "~/Downloads")))
         ("C-x H" . (lambda () (interactive) (dired "~")))
         ("C-x K" . kill-current-buffer)
         ("C-x M-x" . switch-to-minibuffer)
         ("C-x a /" . unexpand-abbrev)
         ("C-x t T" . tab-bar-mode)
         ("M-SPC" . cycle-spacing)
         ("M-s h U" . unhighlight-all-regexp)
         ;; Move this to C-c c w
         ;; ("M-=" . nil)
         :map my-ctl-c-D-map
         ("." . benchmark-this)
         ("T" . cancel-debug-on-entry)
         ("V" . cancel-debug-on-variable-change)
         ("e" . toggle-debug-on-error)
         ("p p" . profiler-toggle)
         ("p r" . profiler-report)
         ("q" . toggle-debug-on-quit)
         ("t" . debug-on-entry)
         ("v" . debug-on-variable-change)
         :map my-ctl-c-c-map
         ("w" . count-words-region)
         :map my-ctl-c-d-map
         ("l" . dictionary-search)
         :map my-ctl-c-e-map
         ("c" . calendar)
         ("C" . year-calendar)
         ("s" . set-variable)
         :map my-ctl-c-f-map
         ("." . ffap)
         ("i" . find-init-file)
         ("s" . find-scratch-buffer)
         :map my-ctl-c-h-map
         ("l" . hl-line-mode)
         ("s" . toggle-hscroll-mode)
         :map my-ctl-c-i-map
         ("SPC" . indent-using-spaces-and-fixup)
         ("TAB" . indent-using-tabs-and-fixup)
         :map my-ctl-c-l-map
         ("d" . duplicate-line)
         :map my-ctl-c-o-map
         ("v" . visible-mode)
         :map my-ctl-c-v-map
         ("f" . visual-line-fill-column-mode)
         ("#" . sudo-find-alternate-file)
         :map my-ctl-c-w-map
         ("'" . insert-pair)
         ("<" . insert-pair)
         ("[" . insert-pair)
         ("\"" . insert-pair)
         :map my-ctl-c-y-map
         ("o" . my-yank-to-other-window))

  :hook ((minibuffer-setup-hook . (lambda () (setq truncate-lines t)))
         ;; Tabs handling
         (shell-mode-hook . set-indent-tab-width-8)
         (emacs-lisp-mode-hook . set-indent-tab-width-2)
         ;; simple.el
         (before-save-hook . delete-trailing-whitespace)
         (emacs-lisp-mode-hook . no-indent-tabs-mode))

  :custom
  ;; abbrev.el
  (abbrev-file-name (expand-file-name "abbrev-defs.el" user-emacs-directory))
  ;; auth-source.el
  (auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
  ;; bindings.el
  (mode-line-percent-position nil)
  ;; bookmark.el
  (bookmark-save-flag 1)
  ;; files.el
  (auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save-files/") t)))
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/"))))
  (confirm-kill-emacs #'yes-or-no-p)
  (large-file-warning-threshold nil)
  (require-final-newline t)
  (revert-without-query '("^.*\\.pdf$"))
  (save-abbrevs 'silently)
  ;; frame.el
  (window-divider-default-right-width 3)
  (window-divider-mode t)
  ;; help.el
  (help-window-select t)
  ;; indent.el
  (tab-always-indent 'complete)
  ;; minibuffer.el
  (completions-format 'one-column)
  ;; paren.el
  (show-paren-delay 0)
  (show-paren-style 'parenthesis)
  (show-paren-when-point-inside-paren nil)
  ;; register.el
  (register-preview-delay 0)
  ;; simple.el
  (eval-expression-print-length nil) ; don't truncate when evaluating exprs
  ;; (eval-expression-print-level nil)  ; print expressions entirely
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  (next-line-add-newlines t)
  (save-interprogram-paste-before-kill nil)
  (set-mark-command-repeat-pop t)
  ;; startup.el
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "")
  (user-mail-address "nicolaisingh@pm.me")
  ;; tab-bar.el
  (tab-bar-auto-width-max '((200) 20))
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs-groups
                    tab-bar-separator
                    tab-bar-format-align-right
                    tab-bar-format-global))
  (tab-bar-show t)
  (tab-bar-tab-hints t)
  ;; time.el
  (display-time-24hr-format t)
  (display-time-day-and-date nil)
  (display-time-default-load-average 0)
  ;; vc.el
  (vc-follow-symlinks t)
  ;; window.el
  (same-window-regexps '("^magit: .*$"
                         "^magit-status: .*$"))

  :init
  ;; Load custom.el
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

  (keymap-global-set "C-x C-m" (key-binding (kbd "M-x"))) ; Does not work in :bind

  (setq auto-hscroll-mode 'current-line
        auto-save-interval 50
        auto-save-no-message t
        auto-save-timeout 3
        disabled-command-function nil
        history-delete-duplicates t
        scroll-margin 0
        tab-width 4
        truncate-lines t
        user-full-name "Nicolai Singh"
        x-stretch-cursor t)

  ;; (if (boundp 'use-short-answers)
  ;;     (setq use-short-answers t)
  ;;   (fset 'yes-or-no-p 'y-or-n-p))

  ;; Recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode)

  ;; Set emacs source code location
  (unless (memq window-system '(mac ns))
    (setq find-function-C-source-directory "/run/current-system/sw/share/emacs/source/src")
    (visit-tags-table (format "/run/current-system/sw/share/emacs/%s/lisp/TAGS" emacs-version)))

  (display-time-mode)
  ;; (global-visual-wrap-prefix-mode) ;; Some incompatibility with org-mode
  (savehist-mode)
  (winner-mode))


;;; 10 diminish

(use-package diminish
  :ensure (:wait t) ; wait to finish before proceeding to the next
  :demand t)


;;; 10 personal-theme

(use-package personal-2-theme
  :demand t
  :ensure nil
  :config
  (enable-theme 'personal-2))


;;; 99 exec-path-from-shell

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :disabled
    :config
    (exec-path-from-shell-initialize)))


;;; aggressive-indent

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :bind (:map my-ctl-c-i-map
              ("A" . aggressive-indent-mode))
  :hook ((clojure-mode-hook . aggressive-indent-mode)
         (clojurescript-mode-hook . aggressive-indent-mode)
         (emacs-lisp-mode-hook . aggressive-indent-mode)
         (js2-mode-hook . aggressive-indent-mode)
         (scheme-mode-hook . aggressive-indent-mode)
         ;; Turn off in the edebug eval list buffer
         (edebug-eval-mode-hook . (lambda () (aggressive-indent-mode -1)))))


;;; aider

(use-package aider
  :bind (("C-c a" . aider-transient-menu))
  :custom
  (aider-todo-keyword-pair '("AI!" . "comment line ending with string: AI!"))
  :config
  (defun my-aider-run-aider (orig-fn edit-args)
    "Call `aider-run-aider' with needed args and logging disabled."
    (interactive "P")
    (setenv "OPENAI_API_KEY" (auth-source-pick-first-password :host "api.openai.com"))
    (setenv "DEEPSEEK_API_KEY" (auth-source-pick-first-password :host "api.deepseek.com"))
    (let* (;; Ordering based on Aider LLM leaderboard (https://aider.chat/docs/leaderboards/)
           (args-openai-o3-pro       '("--model" "openai/o3-pro"))
           (args-openai-o3-high      '("--model" "openai/o3" "--reasoning-effort" "high"))
           (args-openai-o3           '("--model" "openai/o3"))
           (args-openai-o4-mini-high '("--model" "openai/o4-mini" "--reasoning-effort" "high"))
           (args-deepseek-r1         '("--model" "deepseek/deepseek-reasoner"))
           (args-openai-o3-mini-high '("--model" "openai/o3-mini" "--reasoning-effort" "high"))
           (args-deepseek-v3         '("--model" "deepseek/deepseek-chat"))

           ;; Other options
           (args-other               '("--cache-prompts"
                                       "--no-analytics"
                                       ;; "--no-auto-commits"
                                       "--no-gitignore"
                                       "--no-stream"
                                       "--notifications"))
           (aider-args `(,@args-deepseek-r1
                         ,@args-other))
           (message-log-max nil)
           (inhibit-message t))
      (funcall orig-fn edit-args)))
  (advice-add 'aider-run-aider :around #'my-aider-run-aider))


;;; alert

(use-package alert
  :custom
  (alert-fade-time 10)
  :config
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
                          (alert-legacy-log-notify mes sev len)))))


;;; autorevert

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode))


;;; avy

(use-package avy
  :bind (:map my-ctl-z-map
              ("C-v" . avy-goto-char-timer))
  :custom
  (avy-timeout-seconds 0.4)
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))


;;; blacken

(use-package blacken
  :diminish blacken-mode
  :hook (python-mode-hook))


;;; browse-kill-ring

(use-package browse-kill-ring
  :bind (:map my-ctl-c-b-map ("k" . browse-kill-ring)))


;;; calendar

(use-package calendar
  :ensure nil
  :bind (:repeat-map calendar-mode-repeat-map
                     ("]" . calendar-forward-year)
                     ("[" . calendar-backward-year))
  :hook ((calendar-today-visible-hook . calendar-mark-today))
  :config
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

  (with-eval-after-load 'org
    (keymap-set org-mode-map "C-c o >" #'my-calendar-mark-org-headings)))


;;; calibre

(use-package calibre
  :bind (("C-c L" . calibre-library))
  :custom (calibre-libraries '(("library" . "~/calibre"))))


;;; cape

(use-package cape
  :bind (:map my-ctl-c-p-map
              ("TAB" . completion-at-point)
              ;; ("M-TAB" . completion-at-point)
              ("p" . completion-at-point)
              ("t" . complete-tag)
              ("d" . cape-dabbrev)
              ("h" . cape-history)
              ("f" . cape-file)
              ("s" . cape-elisp-symbol)
              ("e" . cape-elisp-block)
              ("a" . cape-abbrev)
              ("l" . cape-line)
              ("w" . cape-dict)
              ("k" . cape-keyword)
              (":" . cape-emoji)
              ("\\" . ape-tex)
              ("_" . cape-tex)
              ("^" . cape-tex)
              ("&" . cape-sgml)
              ("r" . cape-rfc1345))
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-line)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))


;;; centered-cursor-mode

(use-package centered-cursor-mode)


;;; chatgpt-shell

(use-package chatgpt-shell
  :disabled
  :bind (:map
         my-ctl-c-l-map
         ("P" . (lambda () (interactive) (message "%s" (chatgpt-shell-system-prompt))))
         ("SPC" . chatgpt-shell-send-region)
         ("c" . chatgpt-shell-prompt-compose)
         ("f" . chatgpt-shell-fix-error-at-point)
         ("i" . chatgpt-shell-quick-insert)
         ("l" . chatgpt-shell)
         ("p" . chatgpt-shell-proofread-region)
         ("r" . chatgpt-shell-refactor-code)
         ("t" . chatgpt-shell-generate-unit-test)
         ("x i" . chatgpt-shell-describe-image)
         ("x x" . chatgpt-shell-describe-code)
         :map
         chatgpt-shell-mode-map
         ("C-c <backspace>" . chatgpt-shell-delete-interaction-at-point)
         ("C-c C-<backspace>" . chatgpt-shell-clear-buffer)
         ("C-c C-S-p" . chatgpt-shell-load-awesome-prompts)
         ("C-c C-o" . nil) ;; unbind comint-delete-output
         :map
         chatgpt-shell-prompt-compose-mode-map
         ("C-c C-<backspace>" . chatgpt-shell-prompt-compose-clear-history))
  :custom
  (chatgpt-shell-always-create-new nil)
  (chatgpt-shell-models (append (chatgpt-shell-deepseek-models)
                                (chatgpt-shell-openai-models)))
  (chatgpt-shell-model-temperature 0)
  (chatgpt-shell-model-version "deepseek-chat")
  (chatgpt-shell-prompt-query-response-style 'other-buffer)
  (chatgpt-shell-system-prompt (seq-position (map-keys chatgpt-shell-system-prompts) "Programming"))
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-openai-key (lambda ()
                              (auth-source-pick-first-password :host "api.openai.com")))
  (chatgpt-shell-deepseek-key (lambda ()
                                (auth-source-pick-first-password :host "api.deepseek.com")))
  :config
  (add-to-list
   'chatgpt-shell-system-prompts
   `("Family Travel Planning" .
     ,(string-join
       '("The user is planning for a vacation with family and kid/s."
         "Suggest an itinerary that considers planned activities by the user."
         "Also include in the itinerary flight schedules, commuting, walking, and rest times."
         "Mention places that are known to be worth visiting and seeing."
         "Don't suggest activities and destinations that are not family-friendly."
         "Give me up to a max of 10 links to blogs or information that I can review."
         "The itinerary destinations preferrably should be sequenced such that it is travel-friendly."
         "If you will suggest a destination that is quite far from the others, mention it explicitly."
         "When you make the itinerary, include the travel times and an estimate of how long to stay on that area before going to the next one.")
       "  "))
   t))


;;; chess

(use-package chess)


;;; chronos

(use-package chronos
  :bind (("C-c T" . chronos-load))
  :custom
  (chronos-standard-timers '("0:0:30/30-second finished"
                             "5/5-minute timer finished"
                             "15/15-minute timer finished"
                             "30/30-minutes timer finished"
                             "01:00/1-hour timer finished"))
  (chronos-expiry-functions '(chronos-message-notify
                              ;; For macOS: Use chronos-alert instead of chronos-desktop-notifications-notify
                              chronos-desktop-notifications-notify))
  :config
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

  (defun chronos-load ()
    "Load chronos."
    (interactive)
    (if (get-buffer chronos-buffer-name)
        (switch-to-buffer chronos-buffer-name)
      (chronos-initialize))))


;;; clojure-mode

(use-package clojure-mode)


;;; command-log-mode

(use-package command-log-mode
  :bind (("C-c c l" . clm/toggle-command-log-buffer))
  :custom
  (command-log-mode-is-global t)
  (command-log-mode-key-binding-open-log nil)
  (command-log-mode-open-log-turns-on-mode t)
  (command-log-mode-window-font-size 1)
  (command-log-mode-window-size 60)
  :config
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
      (set-window-dedicated-p new-win t))))


;;; completion-preview

(use-package completion-preview
  :ensure nil
  :demand t
  :diminish
  :custom
  (completion-preview-idle-delay 0.2)
  :config
  (defun turn-off-completion-preview-mode ()
    (interactive)
    (completion-preview-mode -1))

  (global-completion-preview-mode))


;;; conf-mode

(use-package conf-mode
  :ensure nil
  :mode ("\\.env.*\\'" . conf-mode))


;;; consult

(use-package consult
  :demand t
  :bind (([remap repeat-complex-command] . consult-complex-command)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap goto-line] . consult-goto-line)
         :map my-meta-o-map
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("k" . consult-keep-lines)
         ("f" . consult-focus-lines)
         ("g" . consult-ripgrep)
         ("G" . consult-git-grep)
         :map my-ctl-c-m-map
         ("M-x" . consult-mode-command)
         :map goto-map
         ("e" . consult-compile-error)
         ("f" . consult-flymake)
         ("I" . consult-imenu-multi)
         ("M" . consult-global-mark)
         ("h" . consult-history)
         ("i" . consult-imenu)
         ("m" . consult-mark)
         :map minibuffer-local-map
         ("M-r" . consult-history)
         ("M-s" . consult-history)
         :map isearch-mode-map
         ("M-o L" . consult-line-multi)
         ("M-o h" . consult-isearch-history)
         ("M-o l" . consult-line))

  :custom
  (consult-async-min-input 2)
  (consult-narrow-key "<")
  (consult-preview-key "C-SPC")

  :config
  (with-eval-after-load 'org
    (let ((map org-mode-map))
      (keymap-set map "C-c *" #'consult-org-heading))))


;;; consult-dir

(use-package consult-dir
  :demand t
  :bind (:map goto-map
              ("d" . consult-dir)
              :map minibuffer-local-map
              ("M-g d" . consult-dir)
              ("M-g f" . consult-dir-jump-file))
  :custom
  (consult-dir-default-command #'consult-dir-dired))


;;; consult-org-roam

(use-package consult-org-roam
  :disabled
  :after (org-roam)
  :bind (("C-c n f" . consult-org-roam-file-find)
         ("C-c n g" . consult-org-roam-search)
         :map org-mode-map
         ("C-c n <" . consult-org-roam-backlinks)
         ("C-c n >" . consult-org-roam-forward-links))
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-after-buffers nil)
  :config
  (consult-org-roam-mode 1))


;;; copilot
(use-package copilot
  :disabled
  :hook ((copilot-mode-hook . turn-off-completion-preview-mode)
         (python-mode-hook . copilot-mode)
         (typescript-ts-mode-hook . copilot-mode))
  :bind (:map
         copilot-completion-map
         ("<tab>" . copilot-accept-completion)
         ("C-w" . copilot-accept-completion-by-word)
         ("C-M-w" . copilot-accept-completion-by-line)
         ("C-." . copilot-next-completion)
         ("C-," . copilot-next-completion))
  :custom
  (copilot-idle-delay 0.5)
  :config
  (add-to-list 'copilot-major-mode-alist '("typescript-ts" . "typescript"))
  (add-to-list 'copilot-major-mode-alist '("yaml-ts" . "yaml")))


;;; corfu

(use-package corfu
  :demand t
  :bind (:map corfu-map
              ("C-s" . corfu-next)
              ("C-." . corfu-next)
              ("C-r" . corfu-previous)
              ("C-," . corfu-previous))
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  :init
  (global-corfu-mode))

(use-package corfu-echo
  :after (corfu)
  :demand t
  :ensure nil
  :custom
  (corfu-echo-delay '(1.0 . 0.5))
  :init
  (corfu-echo-mode))


;;; cov

(use-package cov
  :after (python)
  :bind (:map python-mode-map
              ("C-c C C" . my-python-run-coverage))
  :custom
  (cov-coverage-mode t)
  :config
  (defun my-python-run-coverage ()
    "Interpret the .coverage file in the project root and enable display code coverage."
    (interactive)
    (if current-prefix-arg
        (progn
          (let ((cmd (format "cd %s; coverage json" (project-root (project-current)))))
            (call-process-shell-command cmd))
          (cov-mode 1))
      (call-interactively #'cov-mode))))


;;; csv-mode

(use-package csv-mode)


;;; currency-convert

(use-package currency-convert)


;;; deadgrep

(use-package deadgrep
  :bind (:map deadgrep-mode-map
              ("C-o" . my-deadgrep-open-result-other-window))
  :config
  (defun my-deadgrep-open-result-other-window ()
    (interactive)
    (let* ((deadgrep-buffer (current-buffer))
           (deadgrep-window (get-buffer-window deadgrep-buffer)))
      (select-window (next-window (get-buffer-window deadgrep-buffer)))
      (set-buffer deadgrep-buffer)
      (deadgrep-visit-result)
      (select-window deadgrep-window))))


;;; devdocs

(use-package devdocs
  :bind (("C-h D I" . devdocs-install)
         ("C-h D D" . devdocs-lookup)))


;;; dictionary

(use-package dictionary
  :ensure nil
  :custom
  (dictionary-server "dict.org"))


;;; diff-hl

(use-package diff-hl
  :demand t
  :bind (:map my-ctl-c-d-map
              ("h h" . global-diff-hl-mode)
              ("h a" . #'diff-hl-amend-mode)
              ("h f" . #'diff-hl-flydiff-mode))
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (global-diff-hl-mode 1))


;;; dired

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-." . my-dired-toggle-other-files-visibility)
              ("C-c C-a" . org-attach-dired-to-subtree)
              ("C-c M A" . emms-play-dired)
              ("C-c M a" . emms-add-dired)
              ("C-c d d" . dired-ediff-a-b)
              ("C-c m !" . dired-apply-to-marked-files)
              ("C-c m d" . dired-ediff-marked-files)
              ("C-c m e" . dired-create-empty-file)
              ("z" . dired-up-directory))
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode))
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-information-lines nil)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-isearch-filenames t)
  (dired-kill-when-opening-new-dired-buffer nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  (defvar my-dired-listing-switches "--group-directories-first -lhv")
  (defvar my-dired-listing-a-switch "")

  (when (eq system-type 'darwin)
    ;; Don't forget `brew install coreutils' in MacOS
    (setopt insert-directory-program "gls"))

  (defun my-dired-set-listing-switches ()
    "Update `dired-listing-switches'."
    (setq dired-listing-switches (concat my-dired-listing-switches
                                         my-dired-listing-a-switch)))
  (my-dired-set-listing-switches)

  (defun my-dired-toggle-other-files-visibility ()
    (interactive)
    (setq my-dired-listing-a-switch (if (string-empty-p my-dired-listing-a-switch) " -a" ""))
    (message "%s other files" (if (string-empty-p my-dired-listing-a-switch) "Hide" "Show"))
    (dired-sort-other (my-dired-set-listing-switches)))

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

  ;; dired-ediff-a-b

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
        (ediff-files dired-ediff-file-a dired-ediff-file-b)))))


;;; dired-marked

(use-package dired-marked
  :after (dired)
  :ensure nil
  :bind (:map dired-mode-map
              ("* C" . dired-marked-copy-file-to-marked-directories)
              ("* U" . dired-marked-unmark-all)))


;;; dired-narrow (from dired-hacks)

(use-package dired-narrow
  :after (dired)
  :bind (:map dired-mode-map
              ("/ /" . dired-narrow)
              ("/ r" . dired-narrow-regexp))
  :config
  (setq dired-narrow-map nil))


;;; dired-sidebar

(use-package dired-sidebar
  :bind (:map my-ctl-c-d-map
              ("s" . dired-sidebar-toggle-sidebar)
              ("S" . dired-sidebar-jump-to-sidebar)
              :map dired-sidebar-mode-map
              ("z" . dired-sidebar-up-directory))
  :hook ((dired-sidebar-mode-hook . (lambda ()
                                      (when (file-remote-p default-directory)
                                        (auto-revert-mode -1))))
         (dired-sidebar-mode-hook . (lambda ()
                                      (hl-line-mode -1))))
  :custom
  (dired-sidebar-follow-file-idle-delay 0)
  (dired-sidebar-should-follow-file t)
  (dired-sidebar-theme 'none))


;;; dired-subtree

(use-package dired-subtree
  :after (dired)
  :bind (:map dired-mode-map
              ("<backtab>" . dired-subtree-cycle)
              ("<tab>" . dired-subtree-toggle))
  :custom
  (dired-subtree-use-backgrounds nil)
  :custom-face
  (dired-subtree-depth-1-face ((nil(:background "gray95"))))
  (dired-subtree-depth-2-face ((nil(:background "gray92"))))
  (dired-subtree-depth-3-face ((nil(:background "gray89"))))
  (dired-subtree-depth-4-face ((nil(:background "gray86"))))
  (dired-subtree-depth-5-face ((nil(:background "gray83"))))
  (dired-subtree-depth-6-face ((nil(:background "gray80")))))


;;; dockerfile-mode

(use-package dockerfile-mode)


;;; easy-kill

(use-package easy-kill
  :demand t
  :bind (([remap kill-ring-save] . easy-kill)
         :map easy-kill-base-map
         ("C" . easy-kill-cycle)
         ("M-@" . easy-kill-mark-region)
         ("M-SPC" . easy-kill-mark-region))
  :custom
  (easy-kill-unhighlight-key " ")
  (easy-kill-try-things '(url email uuid sexp line)))


;;; easy-kill-extras

(use-package easy-kill-extras
  :demand t
  :after (easy-kill))

(use-package extra-things
  :after (easy-kill-extras)
  :demand t
  :ensure nil
  :config
  (add-to-list 'easy-kill-alist '(?<  angles-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?>  angles-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\" dquoted-string-universal "") t)
  (add-to-list 'easy-kill-alist '(?\' squoted-string-universal "") t)
  (add-to-list 'easy-kill-alist '(?\( parentheses-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?\) parentheses-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\[ brackets-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?\] brackets-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\` bquoted-string-universal "") t)
  (add-to-list 'easy-kill-alist '(?{  curlies-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?}  curlies-pair-content "\n") t))


;;; eat

(use-package eat
  :diminish eat-eshell-mode
  :hook ((eshell-load-hook . eat-eshell-mode)
         (eshell-load-hook . eat-eshell-visual-command-mode)))


;;; ediff

(use-package ediff
  :ensure nil
  :bind (:map my-ctl-c-d-map
              ("." . ediff-current-file)
              ("f" . ediff-files)
              ("k" . ediff-last-2-kills)
              ("r l" . ediff-regions-linewise)
              ("r w" . ediff-regions-wordwise)
              ("w" . ediff-buffers-in-window)
              ("b" . ediff-buffers))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :hook ((ediff-before-setup-hook . ediff-save-windows-config)
         (ediff-quit-hook . ediff-restore-window-config))
  :init
  (defun ediff-save-windows-config ()
    "Save current window configuration in `ediff-previous-window-config'."
    (setq ediff-previous-window-config (current-window-configuration)))

  (defun ediff-restore-window-config ()
    "Restore the saved window configuration in `ediff-previous-window-config'."
    (set-window-configuration ediff-previous-window-config)
    (when (get-buffer ediff-temp-buffer-a) (kill-buffer ediff-temp-buffer-a))
    (when (get-buffer ediff-temp-buffer-b) (kill-buffer ediff-temp-buffer-b)))

  (defun ediff-buffers-in-window ()
    "Run ediff on the current and the next buffer in the window."
    (interactive)
    (ediff-buffers (window-buffer (car (window-list)))
                   (window-buffer (cadr (window-list)))))
  :config
  (defvar ediff-temp-buffer-a "*diff-a*")
  (defvar ediff-temp-buffer-b "*diff-b*")
  (defun ediff-last-2-kills ()
    "Run ediff on the last 2 kills."
    (interactive)
    (let ((text-a (current-kill 1 t))
          (text-b (current-kill 0 t))
          (a (generate-new-buffer (generate-new-buffer-name ediff-temp-buffer-a)))
          (b (generate-new-buffer (generate-new-buffer-name ediff-temp-buffer-b))))
      (with-current-buffer a (insert text-a))
      (with-current-buffer b (insert text-b))
      (ediff-buffers a b))))


;;; edit-indirect

(use-package edit-indirect
  :bind (("C-c '" . edit-indirect-region)))


;;; eglot

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  :config
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
  (setq-mode-local js-mode
                   eglot-ignored-server-capabilities '(:documentHighlightProvider
                                                       :hoverProvider
                                                       :inlayHintProvider)))


;;; eldoc

(use-package eldoc
  :demand t
  :ensure nil
  :diminish eldoc-mode)


;;; emms

(use-package emms
  :ensure (:source "GNU ELPA")
  :bind (([remap emms-playlist-save] . my-emms-playlist-save)
         :map my-ctl-c-M-map
         ("%" . my-emms-toggle-random-playlist)
         ("1" . my-emms-toggle-repeat-track)
         ("B" . emms-smart-browse)
         ("P" . emms-pause)
         ("b" . my-emms-metaplaylist-mode-go)
         ("i" . emms-show-all)
         ("l" . emms-playlist-mode-switch-buffer)
         ("m" . emms-mode-line-mode)
         ("n" . emms-next)
         ("p" . emms-previous)
         ("s" . emms-stop)
         :map emms-browser-mode-map
         ("R" . emms-browser-replace-playlist)
         ("a" . emms-add-directory-tree)
         :map emms-mark-mode-map
         ("M" . emms-mark-mode-disable)
         :map emms-metaplaylist-mode-map
         ("G" . my-emms-metaplaylist-find-all-playlists)
         ("f" . my-emms-metaplaylist-find-playlist)
         ("v" . emms-metaplaylist-mode-goto)
         ("q" . kill-current-buffer)
         ("z" . emms-playlist-mode-go)
         :map emms-playlist-mode-map
         ("%" . emms-shuffle)
         ("&" . my-emms-toggle-loop)
         ("," . emms-seek-backward)
         ("." . emms-seek-forward)
         ("<left>" . emms-seek-backward)
         ("<right>" . emms-seek-forward)
         ("A" . my-emms-add-track-to-playlist)
         ("C-x C-w" . my-emms-playlist-write)
         ("F" . emms-show-all)
         ("M" . emms-mark-mode)
         ("i" . my-emms-insert-track-to-playlist)
         ("z" . emms-metaplaylist-mode-go))
  :custom
  (emms-player-list '(emms-player-mpv
                      emms-player-alsaplayer
                      emms-player-mpg321
                      emms-player-ogg123))
  (emms-repeat-playlist t)
  (emms-seek-seconds 5)
  (emms-track-description-function #'my-emms-track-description)

  :config
  (defvar my-emms-insert-track-to-playlist-destination emms-playlist-buffer-name
    "What playlist to use in `my-emms-insert-track-to-playlist'.")
  (defvar my-emms-loop-timer nil)

  (defun my-emms-track-description (track)
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
           (format "(%02d:%02d)  " (/ duration 60) (% duration 60))
           (when (stringp discnumber) (format "%s." discnumber))
           (when (numberp discnumber) (format "%d." discnumber))
           (when (stringp tracknumber) (format "%s. " tracknumber))
           (when (numberp tracknumber) (format "%d. " tracknumber))
           (when artist (format "%s - " artist))
           title)
        (emms-track-simple-description track))))

  (defun my-emms-toggle-random-playlist ()
    (interactive)
    (emms-toggle-random-playlist)
    (setq emms-repeat-track nil)
    (emms-mode-line-alter))

  (defun my-emms-toggle-repeat-track ()
    (interactive)
    (emms-toggle-repeat-track)
    (when emms-repeat-track
      (customize-set-variable 'emms-random-playlist nil))
    (emms-mode-line-alter))

  (defun my-emms-add-track-to-playlist (buffer)
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

  (defun my-emms-insert-track-to-playlist ()
    "Add the current track at point to the playlist set in `my-emms-insert-track-to-playlist-destination'.
When a prefix is used, ask where to insert the track and place it there."
    (interactive)
    (let* ((name (emms-track-get (emms-playlist-track-at) 'name))
           (prev-playlist emms-playlist-buffer)
           (dest-playlist (if (not current-prefix-arg)
                              my-emms-insert-track-to-playlist-destination
                            (completing-read
                             "Insert track in: "
                             (mapcar #'buffer-name emms-playlist-buffers) nil t))))

      (unless (eq my-emms-insert-track-to-playlist-destination dest-playlist)
        (setq my-emms-insert-track-to-playlist-destination dest-playlist))
      (emms-playlist-set-playlist-buffer dest-playlist)
      (emms-insert-file name)
      (emms-playlist-set-playlist-buffer prev-playlist)
      (message "Added to: %s" dest-playlist)))

  (defun my-emms-stop-loop ()
    "Stop the currently running loop."
    (interactive)
    (when (timerp my-emms-loop-timer)
      (message "Stopping EMMS loop")
      (cancel-timer my-emms-loop-timer)))

  (defun my-emms-toggle-loop (arg)
    "Continuously loop a the last ARG seconds of the currently playing track.
Use a negative argument to stop the loop."
    (interactive "p")
    (my-emms-stop-loop)
    (when (>= arg 1)
      (let ((length (if (= 1 arg) 5 arg)))
        (message "Starting EMMS loop of %s seconds" length)
        (setq my-emms-loop-timer (run-with-timer 0 length (lambda () (emms-seek (- length)))))))))

;; Browsing

;;; emms-browser

(use-package emms-browser
  :ensure nil
  :after (emms)
  :demand t
  :hook ((emms-browser-mode-hook . hl-line-mode))
  ;; :custom
  ;; (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  :config
  (emms-browser-make-filter "all" 'ignore)
  (emms-browser-make-filter "flac" (emms-browser-filter-only-dir my-music-flac-directory))
  (emms-browser-make-filter "library" (emms-browser-filter-only-dir my-music-library-directory))
  (emms-browser-set-filter (assoc "library" emms-browser-filters)))

;;; emms-playlist-mode

(use-package emms-playlist-mode
  :ensure nil
  :after (emms)
  :demand t
  :hook ((emms-playlist-mode-hook . hl-line-mode))
  :custom
  (emms-playlist-mode-open-playlists t))

;;; emms-metaplaylist-mode

(use-package emms-metaplaylist-mode
  :ensure nil
  :after (emms)
  :demand t
  :config
  (defun my-emms-metaplaylist-find-playlist (file &optional skip-update)
    (interactive (list (read-file-name "Find playlist file: "
                                       my-music-playlist-directory
                                       my-music-playlist-directory
                                       t)))
    (let* ((filename (file-name-nondirectory file))
           (playlist-name (concat " *EMMS Playlist: " filename "*"))
           (previous-buffer emms-playlist-buffer))
      (unless (get-buffer playlist-name)
        (emms-metaplaylist-mode-new-buffer playlist-name)
        (emms-playlist-set-playlist-buffer playlist-name)
        (emms-insert-playlist file)
        (unless skip-update
          (emms-playlist-set-playlist-buffer previous-buffer)
          (emms-metaplaylist-mode-update)))))

  (defun my-emms-metaplaylist-find-all-playlists ()
    (interactive)
    (let ((directory (if current-prefix-arg
                         (read-directory-name "Find all playlists in: "
                                              my-music-playlist-directory
                                              my-music-playlist-directory
                                              t)
                       my-music-playlist-directory))
          (previous-buffer emms-playlist-buffer))
      (message "Finding all playlist files at %s" directory)
      (dolist (file (directory-files directory t directory-files-no-dot-files-regexp))
        (my-emms-metaplaylist-find-playlist file t))
      (emms-playlist-set-playlist-buffer previous-buffer)
      (emms-metaplaylist-mode-update)))

  (defun my-emms-metaplaylist-mode-go ()
    (interactive)
    (when (not (emms-playlist-buffer-list))
      ;; Create an empty playlist buffer if none exist yet
      (emms-playlist-current-clear)
      (my-emms-metaplaylist-find-all-playlists))
    (emms-metaplaylist-mode-go)))

;;; emms-playlist-limit

(use-package emms-playlist-limit
  :ensure nil
  :after (emms)
  :demand t)

;; Playback

;;; emms-player-simple

(use-package emms-player-simple :ensure nil :after (emms) :demand t)

;;; emms-player-mpv

(use-package emms-player-mpv :ensure nil :after (emms) :demand t)

;; Track information

;;; emms-source-file

(use-package emms-source-file
  :ensure nil
  :after (emms)
  :demand t)

;;; emms-source-playlist

(use-package emms-source-playlist
  :ensure nil
  :after (emms)
  :demand t
  :custom
  (emms-source-playlist-default-format 'm3u)
  :config
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

  (defun my-emms-playlist-write ()
    (interactive)
    (let ((emms-source-file-default-directory my-music-playlist-directory))
      (call-interactively #'emms-playlist-write)))

  (defun my-emms-playlist-save ()
    (interactive)
    (let ((emms-source-file-default-directory my-music-playlist-directory))
      (call-interactively #'emms-playlist-save))))

;;; emms-info

(use-package emms-info
  :ensure nil
  :after (emms)
  :demand t
  :custom
  (emms-info-asynchronously nil)
  (emms-info-functions '(emms-info-native emms-info-cueinfo))
  :config
  (add-to-list 'emms-track-initialize-functions #'emms-info-initialize-track))

;;; emms-cue

(use-package emms-cue :ensure nil :after (emms) :demand t)

;;; emms-info-native

(use-package emms-info-native :ensure nil :after (emms) :demand t)

;;; emms-show-all

(use-package emms-show-all :ensure nil :after (emms) :demand t)

;; Caching (persists music library)

;;; emms-cache

(use-package emms-cache
  :ensure nil
  :after (emms)
  :demand t
  :config
  (emms-cache 1))

;; Others

;;; emms-mode-line

(use-package emms-mode-line
  :ensure nil
  :after (emms)
  :demand t
  :hook ((emms-player-paused-hook . emms-mode-line-alter))
  :custom
  (emms-mode-line-mode-line-function #'my-emms-mode-line-display)
  :config
  (defun my-emms-mode-line-display ()
    (concat " ["
            (buffer-name emms-playlist-buffer)
            (if emms-player-paused-p " paused")
            (if (with-current-emms-playlist emms-random-playlist) " random" "")
            (if (with-current-emms-playlist emms-repeat-track) " repeat-track" "")
            " ] "))
  (emms-mode-line-mode))

;;; emms-later-do

(use-package emms-later-do
  :ensure nil
  :after (emms)
  :demand t
  :custom
  (emms-later-do-interval 0.001))


;;; emojify

(use-package emojify)


;;; epa

(use-package epa
  :ensure nil
  :custom
  (epa-keys-select-method 'minibuffer))


;;; epg

(use-package epg
  :ensure nil
  :custom
  (epg-pinentry-mode 'loopback))


;;; erc

(use-package erc
  :ensure nil
  :config
  (erc-dcc-mode 1))


;;; eshell

(use-package eshell
  :ensure nil
  :bind (:map
         my-ctl-c-e-map
         ("e" . eshell)
         ("E" . eshell-other)
         :map
         eshell-mode-map
         ("C-c ?" . chatgpt-shell-eshell-whats-wrong-with-last-command)
         ("C-c !" . chatgpt-shell-eshell-summarize-last-command-output))
  :custom
  (eshell-hist-ignoredups t)
  (eshell-history-size 10000)
  (eshell-ls-dired-initial-args '("-h"))
  (eshell-ls-initial-args '("-h"))
  :hook ((eshell-mode-hook . my-eshell-config))
  :init
  (defun my-eshell-config ()
    (setq-local completion-auto-help t)
    (setenv "PAGER" "cat")
    (keymap-set eshell-mode-map "C-c C-<backspace>" (lambda ()
                                                      (interactive)
                                                      (eshell/clear-scrollback))))
  (defun eshell-other ()
    (interactive)
    (let ((eshell-buffer-name "*eshell-other*"))
      (eshell)))
  :config
  ;; (add-to-list 'eshell-modules-list 'eshell-rebind)
  (add-to-list 'eshell-modules-list 'eshell-xtra))


;;; eshell-toggle

(use-package eshell-toggle
  :bind (:map my-ctl-z-map
              ("C-s" . my-eshell-toggle))
  :custom
  (eshell-toggle-run-command nil)
  ;; (eshell-toggle-size-fraction 2)
  :config
  (defun eshell-toggle--make-buffer-name ()
    eshell-buffer-name)

  (defun my-eshell-toggle (arg)
    "Toggle eshell, and cd to buffer directory if a prefix ARG is given."
    (interactive "P")
    (if arg
        (my-eshell-toggle-cd)
      (eshell-toggle)))

  (defun my-eshell-toggle-cd ()
    (interactive)
    (let ((buffer-dir default-directory))
      (eshell-toggle)
      (when (and eshell-toggle--toggle-buffer-p
                 (not (string-equal default-directory buffer-dir)))
        (insert (format "cd %s" buffer-dir))
        (eshell-send-input)))))


;;; eshell-up

(use-package eshell-up
  :after (eshell)
  :demand t)


;;; eshell-z

(use-package eshell-z
  :after (eshell)
  :demand t)


;;; expand-region

(use-package expand-region
  :bind (:map my-ctl-c-e-map
              ("r" . er/expand-region)
              ("[" . er/mark-inside-pairs)
              ("{" . er/mark-outside-pairs)
              ("'" . er/mark-inside-quotes)
              ("\"" . er/mark-outside-quotes)))


;;; find-dired

(use-package find-dired
  :disabled
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c m f" . find-name-dired-current)
              ("C-c m g g" . find-grep-dired-current))
  :config
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
      (find-grep-dired (dired-current-directory) regexp))))


;;; flymake

(use-package flymake
  :ensure nil
  :custom
  (flymake-no-changes-timeout 1))


;;; focus

(use-package focus)


;;; forge

(use-package forge
  :after (magit))


;;; form-feed

(use-package form-feed
  :diminish form-feed-mode
  :hook ((emacs-lisp-mode-hook . form-feed-mode)
         (howm-view-contents-mode-hook . form-feed-mode)))


;;; git-timemachine

(use-package git-timemachine)


;;; gnus

(use-package gnus
  :ensure nil
  :custom
  (gnus-select-method '(nnnil ""))
  (gnus-secondary-select-methods
   '((nnimap "proton"
             (nnimap-address "127.0.0.1")
             (nnimap-server-port "imap")
             (nnimap-stream plain)
             (nnimap-user "nicolaisingh@pm.me"))))
  (gnus-parameters
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
      (expiry-wait . 56))))

  (gnus-expert-user nil)
  (gnus-inhibit-startup-message t)
  (gnus-interactive-exit 'quiet)
  (gnus-large-newsgroup 1000)
  (gnus-novice-user nil)
  (gnus-summary-line-format "%U%R%z%(%-20,20f%) %2t : %B %1{%[%&user-date;%]%}\n")
  (gnus-use-cache t)
  (gnus-use-trees nil)
  (mm-text-html-renderer 'gnus-w3m)
  (message-signature "Drew")
  (gnus-max-image-proportion 0.5))

(use-package gnus-async
  :ensure nil
  :after (gnus)
  :custom
  (gnus-asynchronous t))

(use-package gnus-cache
  :ensure nil
  :after (gnus)
  :custom
  (gnus-cacheable-groups nil)
  (gnus-uncacheable-groups "^nnml\\|^nnfolder\\|^nnmaildir"))

(use-package gnus-dired
  :ensure nil
  :after (gnus)
  :hook ((dired-mode-hook . gnus-dired-mode)))

(use-package gnus-group
  :ensure nil
  :after (gnus)
  :bind (:map gnus-group-mode-map
              ("v s" . my-gnus-mbsync))
  :hook ((gnus-group-mode-hook . gnus-topic-mode))
  :custom
  (gnus-keep-same-level t)
  (gnus-group-goto-unread t)
  (gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)\n")
  (gnus-group-sort-function '(gnus-group-sort-by-alphabet gnus-group-sort-by-level))
  (gnus-permanently-visible-groups "INBOX")
  :config
  (defun my-gnus-offlineimap-sync ()
    (interactive)
    (async-shell-command "offlineimap -u ttyui" (get-buffer-create "*offlineimap*")))

  (defun my-gnus-mbsync ()
    (interactive)
    (async-shell-command "systemctl start mbsync && journalctl -u mbsync -f" (get-buffer-create "*mbsync*"))))

(use-package gnus-salt
  :ensure nil
  :after (gnus)
  :custom
  (gnus-generate-tree-function #'gnus-generate-horizontal-tree))

(use-package gnus-score
  :ensure nil
  :after (gnus)
  :custom
  (gnus-default-adaptive-score-alist '((gnus-saved-mark (subject 15))
                                       (gnus-replied-mark (subject 10) (from 5))
                                       (gnus-read-mark (subject 5) (from 1))
                                       (gnus-del-mark (subject -5) (from -1))
                                       (gnus-killed-mark (subject -10) (from -5))
                                       (gnus-catchup-mark (subject -15))))
  (gnus-score-expiry-days 90)
  (gnus-use-adaptive-scoring '(line))
  (gnus-adaptive-pretty-print t)
  (gnus-summary-mark-below -50))

(use-package gnus-search
  :ensure nil
  :after (gnus)
  :custom
  (gnus-search-use-parsed-queries nil))

(use-package gnus-start
  :ensure nil
  :after (gnus)
  :custom
  (gnus-always-read-dribble-file t)
  (gnus-subscribe-newsgroup-method 'gnus-subscribe-topics)
  ;; 1-5 : subscribed
  ;; 6-7 : unsubscribed
  ;; 8   : zombie
  ;; 9   : killed
  (gnus-activate-level 2)
  (gnus-level-default-subscribed 3)

  ;; Don't maintain the .newsrc file
  (gnus-read-newsrc-file nil)
  (gnus-save-newsrc-file nil))

(use-package gnus-sum
  :ensure nil
  :after (gnus)
  :bind (:map gnus-summary-mode-map
              ("v <backspace>" . my-gnus-trash-article))
  :custom
  (gnus-auto-select-next nil)
  (gnus-sort-gathered-threads-function #'gnus-thread-sort-by-date)
  (gnus-sum-thread-tree-false-root nil)
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-leaf-with-other "├── ")
  (gnus-sum-thread-tree-root nil)
  (gnus-sum-thread-tree-single-indent nil)
  (gnus-sum-thread-tree-single-indent nil)
  (gnus-sum-thread-tree-single-leaf "└── ")
  (gnus-sum-thread-tree-vertical "│ ")
  (gnus-summary-display-arrow t)
  (gnus-summary-next-group-on-exit nil)
  (gnus-summary-same-subject "")
  (gnus-thread-hide-subtree t)
  (gnus-thread-sort-functions '((not gnus-thread-sort-by-number)))
  :config
  (defun my-gnus-trash-article (arg)
    (interactive "P")
    (cond
     ((string-match "nnimap\\+proton" gnus-newsgroup-name)
      (gnus-summary-move-article arg "nnimap+proton:Trash"))
     (t
      (message "no trash")))))

(use-package gnus-topic
  :ensure nil
  :after (gnus)
  :custom
  (gnus-topic-display-empty-topics t)
  (gnus-topic-line-format "%i[ %(%{%A: %n%}%) ]%v\n"))

(use-package gnus-util
  :ensure nil
  :after (gnus)
  :custom
  (gnus-verbose 5))


;;; go-mode

(use-package go-mode)


;;; goggles

(use-package goggles
  :demand t
  :diminish
  :hook ((prog-mode-hook . goggles-mode)
         (text-mode-hook . goggles-mode)))


;;; gptel

(use-package gptel
  :demand t
  :bind (:map
         my-meta-=-map
         ("=" . gptel-send)
         ("M-+" . gptel-menu)
         ("M-=" . gptel-send)
         ("g" . gptel)
         ("q" . gptel-abort)
         :map gptel-mode-map
         ("M-n" . gptel-end-of-response)
         ("M-p" . gptel-beginning-of-response)
         :repeat-map
         gptel-mode-repeat-map
         ("M-n" . gptel-end-of-response)
         ("M-p" . gptel-beginning-of-response))
  :hook ((gptel-mode-hook . my-gptel-mode-config)
         (gptel-post-response-functions . my-gptel-post-response-config))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-directives (my-gptel-load-prompts))
  (gptel-display-buffer-action '(pop-to-buffer-same-window))
  (gptel-log-level 'info)
  (gptel-org-branching-context t)
  (gptel-rewrite-default-action 'dispatch)
  (gptel-stream nil)
  (gptel-track-media t)
  :init
  (defun my-gptel-mode-config ()
    (setq-local gptel-stream t)
    (font-lock-add-keywords nil `((,(regexp-quote (alist-get 'org-mode gptel-prompt-prefix-alist)) 0
                                   '(:inherit font-lock-builtin-face))
                                  (,(regexp-quote (alist-get 'org-mode gptel-response-prefix-alist)) 0
                                   '(:inherit font-lock-variable-name-face)))))

  (defun my-gptel-post-response-config (beg end)
    (gptel-end-of-response)
    (recenter-top-bottom))

  (defun my-gptel-load-prompts ()
    "Read contents of llm-prompts/ and return an alist according to `gptel-directives'."
    (let* ((dir (expand-file-name "llm-prompts" user-emacs-directory))
           (files (directory-files-recursively dir ".*" nil t nil)))
      (mapcar (lambda (file)
                (when (file-regular-p file)
                  (let ((name (intern
                               (replace-regexp-in-string
                                "/" "-"
                                (file-name-sans-extension
                                 (file-relative-name file dir)))))
                        (prompt (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string))))
                    (cons name prompt))))
              files)))

  (defun my-gptel-update-prompts ()
    "Read llm-prompts/ and update `gptel-directives'."
    (interactive)
    (setopt gptel-directives (my-gptel-load-prompts)))
  :config
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*User*: "
        (alist-get 'org-mode gptel-response-prefix-alist) "*Assistant*:\n")
  (setq gptel-expert-commands t)

  (defun my-gptel-proofread (beg end)
    "Ask gptel to proofread the region."
    (interactive "r")
    (let ((prompt (buffer-substring-no-properties beg end)))
      (gptel-with-preset 'proofread
        (gptel-request prompt
          :in-place t
          :callback (lambda (response info)
                      (when (stringp response)
                        (with-current-buffer (plist-get info :buffer)
                          (delete-region beg end)
                          (goto-char (plist-get info :position))
                          (insert response))))))))

  (defun my-gptel-git-commit-message ()
    "Ask gptel to assist in writing a good git commit message."
    (interactive)
    (let ((staged-diff (magit-git-output "diff" "--cached")))
      (gptel-with-preset 'git-commit
        (gptel-request staged-diff
          :in-place t
          :stream t
          :context nil
          :callback (lambda (response info)
                      (when (stringp response)
                        (with-current-buffer (plist-get info :buffer)
                          (insert response))))))))

  ;; Define backends
  (let ((chatgpt gptel--openai)
        (deepseek (gptel-make-deepseek "DeepSeek"
                    :stream t
                    :key #'gptel-api-key)))
    (setopt gptel-backend deepseek
            gptel-model 'deepseek-chat)))

(use-package gptel-integrations
  :after (gptel)
  :demand t
  :ensure nil)

(use-package gptel-my-presets
  :after (gptel)
  :demand t
  :ensure nil)


;;; gptel-quick

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick" :inherit nil)
  :bind (:map
         my-meta-=-map
         ("M-q" . gptel-quick)))


;;; graphviz-dot-mode

(use-package graphviz-dot-mode)


;;; grep

(use-package grep
  :ensure nil
  :demand t
  :config
  (add-to-list 'grep-find-ignored-directories "node_modules")

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

  (with-eval-after-load 'dired
    (let ((map dired-mode-map))
      (keymap-set map "C-c m g r" #'rgrep-dired)
      (keymap-set map "C-c m g l" #'lgrep-dired))))


;;; highlight-indent-guides

(use-package highlight-indent-guides
  :bind (:map my-ctl-c-h-map
              ("i" . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-dots)
  (highlight-indent-guides-responsive nil)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 20)
  (highlight-indent-guides-auto-top-character-face-perc 40)
  (highlight-indent-guides-auto-stack-character-face-perc 30))


;;; highlight-numbers

(use-package highlight-numbers
  :bind (:map my-ctl-c-h-map
              ("n" . highlight-numbers-mode)))


;;; hippie-exp

(use-package hippie-exp
  :disabled
  :ensure nil
  :bind (("M-/" . hippie-expand))
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-visible
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-expand-all-abbrevs
     try-expand-line
     try-expand-line-all-buffers
     try-complete-file-name-partially
     try-complete-file-name)))


;;; howm

(use-package howm
  :after (org)
  :demand t
  :ensure (:protocol ssh :remotes (("fork" :repo "nicolaisingh/howm"))))

;;; howm-action-lock (action-lock)

(use-package action-lock
  :after (howm)
  :ensure nil
  :diminish action-lock-mode
  :bind (:map
         howm-mode-map
         ("C-z n" . action-lock-goto-next-link)
         ("C-z p" . action-lock-goto-previous-link)
         :map
         howm-menu-mode-map
         ("<backtab>" . action-lock-goto-previous-link)
         :repeat-map
         howm-mode-repeat-map
         ("n" . action-lock-goto-next-link)
         ("p" . action-lock-goto-previous-link))
  :hook ((action-lock-mode-hook . my-action-lock-mode-config))
  :config
  (defvar my-action-lock-checkbox '("[ ]" "[X]" "[-]" "[*]"))
  (defvar my-action-lock-datebox '("[@]" "[%Y-%m-%d %H:%M]"))
  (defvar my-action-lock-jira-issue-regexp "\\(\\(?:JIRA\\|ABCD\\)-[0-9]+\\)")

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
             (my-action-lock-jira-browse-rule my-action-lock-jira-issue-regexp 0))))))

;;; howm-attach

(use-package howm-attach
  :after (howm)
  :ensure nil
  :bind (:map
         howm-mode-map
         ("C-z C-a" . howm-attach)
         ("C-z C-j" . howm-attach-jump-to-dired)
         :map
         dired-mode-map
         ("C-z C-j" . howm-attach-jump-to-note)))

;;; howm-backend

(use-package howm-backend
  :after (howm)
  :ensure nil
  :bind (:map
         howm-mode-map
         ("C-z o" . howm-occur)
         ("C-z x" . howm-list-mark-ring)))

;;; howm-date

(use-package howm-date
  :after (howm)
  :ensure nil
  :bind (("C-z S" . howm-search-past)
         ("C-z d" . howm-insert-date)
         ("C-z t" . howm-insert-dtime)
         :map howm-mode-map
         ("C-z S" . howm-search-past)
         ("C-z d" . howm-insert-date)
         ("C-z t" . howm-insert-dtime)))

;;; howm-menu

(use-package howm-menu
  :after (howm)
  :ensure nil
  :bind (("C-z ." . howm-find-today)
         ("C-z :" . howm-find-yesterday)
         ("C-z m" . howm-menu)
         :map howm-mode-map
         ("C-z ." . howm-find-today)
         ("C-z :" . howm-find-yesterday)
         ("C-z m" . howm-menu)
         :map howm-menu-mode-map
         ("n" . next-line)
         ("p" . previous-line))
  :init
  (setq howm-menu-file-extension ".org"))

;;; howm-misc

(use-package howm-misc
  :after (howm howm-vars)
  :ensure nil
  :diminish howm-org-font-lock-minor-mode
  :bind (("C-z I" . howm-create-interactively)
         ("C-z M" . howm-open-named-file)
         ("C-z SPC" . howm-toggle-buffer)
         ("C-z e" . howm-remember)
         :map
         howm-mode-map
         ("C-z F" . howm-first-memo)
         ("C-z I" . howm-create-interactively)
         ("C-z L" . howm-last-memo)
         ("C-z M" . howm-open-named-file)
         ("C-z N" . howm-next-memo)
         ("C-z P" . howm-previous-memo)
         ("C-z Q" . howm-kill-all)
         ("C-z SPC" . howm-toggle-buffer)
         ("C-z e" . howm-remember)
         ("C-z w" . howm-toggle-narrow)
         :repeat-map
         howm-mode-repeat-map
         ("N" . howm-next-memo)
         ("P" . howm-previous-memo)
         ("F" . howm-first-memo)
         ("L" . howm-last-memo))
  :hook ((howm-create-hook . howm-narrow-to-memo)
         (howm-view-open-hook . howm-narrow-to-memo)
         (org-mode-hook . howm-set-mode)))

;;; howm-mode

(use-package howm-mode
  :after (howm)
  :ensure nil
  :bind (("C-z #" . my-howm-jump-to-last-view)
         ("C-z 0" . my-howm-list-grep-contents)
         ("C-z @" . my-howm-list-grep-tag)
         ("C-z D" . howm-dup)
         ("C-z H" . howm-mode)
         ("C-z K" . howm-keyword-to-kill-ring)
         ("C-z a" . howm-list-all)
         ("C-z c" . howm-create)
         ("C-z g" . howm-list-grep)
         ("C-z h" . howm-history)
         ("C-z i" . howm-insert-keyword)
         ("C-z l" . howm-list-recent)
         ("C-z s" . howm-list-grep-fixed)
         :map howm-mode-map
         ("C-z >" . my-howm-insert-file-ref)
         ("C-z A" . howm-list-around)
         ("C-z C" . howm-create-here)
         ("C-z C-," . my-howm-insert-keyword-header)
         ("C-z C-." . my-howm-insert-ref-header)
         ("C-z D" . howm-dup)
         ("C-z K" . howm-keyword-to-kill-ring)
         ("C-z a" . howm-list-all)
         ("C-z c" . howm-create)
         ("C-z g" . howm-list-grep)
         ("C-z h" . howm-history)
         ("C-z i" . howm-insert-keyword)
         ("C-z k" . my-howm-insert-keywords-line)
         ("C-z l" . howm-list-recent)
         ("C-z r" . howm-refresh)
         ("C-z s" . howm-list-grep-fixed))
  :hook
  ;; Make sure this runs late to make `delete-trailing-whitespace' not remove the trailing header spaces
  ((howm-mode-hook . (lambda ()
                       (add-hook 'before-save-hook #'my-howm-before-save 90 t))))
  :preface
  (setq howm-default-key-table nil
        howm-template #'my-howm-template
        howm-wiki-regexp nil)
  :config
  (advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
  (advice-add 'howm-list-toggle-title :after #'my-howm-message-title-state)
  ;; Advising instead of hooking since the buffer still needs to be narrowed
  (advice-add 'howm-save-buffer :before #'my-howm-add-final-newlines)

  (add-to-list 'howm-template-rules
               '("%dateonly" . (lambda (arg)
                                 (let ((date (format-time-string howm-date-format)))
                                   (insert (format howm-insert-date-format date))))))
  (add-to-list 'howm-template-rules
               '("%notitle" . (lambda (arg)
                                (insert "Notes"))))

  (defun my-howm-jump-to-last-view ()
    (interactive)
    (let ((history-buffer-name (format howm-menu-name-format howm-history-file)))
      (save-excursion
        (howm-history)
        (with-current-buffer history-buffer-name
          (action-lock-magic-return)
          (kill-buffer history-buffer-name)))))

  (defun my-howm-add-final-newlines (&optional arg)
    (save-excursion
      (when (buffer-narrowed-p)
        ;; Ensure prepended note has newlines before previous note header
        ;; Add 2 newlines
        (when (/= (char-after (- (point-max) 1)) ?\n)
          (goto-char (point-max))
          (insert ?\n))
        (when (/= (char-after (- (point-max) 2)) ?\n)
          (goto-char (point-max))
          (insert ?\n)))))

  (defun my-howm-before-save ()
    (my-howm-collect-keywords)
    ;; Ensures old notes have proper titles
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+$" nil t)
        (replace-match (concat (match-string 0) " Notes")))))

  (defun my-howm-message-title-state (&optional undo)
    (message "Titles: %s" (if howm-list-title-previous "On" "Off")))

  (defun my-howm-template (which-template previous-buffer)
    "Howm template chooser."
    (let ((templates `(("default"
                        ,(concat howm-view-title-header " %notitle\n%date %file\n\n%cursor\n\n"))

                       ("Meeting"
                        ,(concat howm-view-title-header " Meeting: %title%cursor\n"
                                 "%date %file\n\n"))

                       ("Meeting - Standup/DSM"
                        ,(concat howm-view-title-header " Meeting: Standup\n"
                                 "%date\n\n"
                                 "%cursor\n\n"))

                       ("Task log"
                        ,(concat howm-view-title-header " Task Log\n"
                                 "%date %dateonly\n\n"
                                 "tasklog%cursor\n\n")))))
      (cond
       ((= which-template 4)
        (let ((choice (completing-read "Template: "
                                       (my-presorted-completion-table (mapcar #'car templates))
                                       nil t nil t)))
          (cadr (assoc choice templates))))
       (t
        (concat howm-view-title-header " %notitle\n%date %file\n\n%cursor\n\n")))))

  (defun my-howm-insert-keywords-line ()
    "Insert keywords line or append if on one."
    (interactive)
    (let* ((completion-table (mapcar #'list (howm-keyword-list)))
           (keywords (completing-read-multiple "Keyword: " completion-table nil nil "@"))
           (method))
      (save-excursion
        (beginning-of-line)
        (cond ((looking-at-p "keywords: ")
               (end-of-line)
               (insert " "))
              ((looking-at-p "^$")
               (insert "keywords: "))
              (t
               (end-of-line)
               (insert "\nkeywords: ")))
        (insert (string-join keywords " ")))))

  (defun my-howm-insert-file-ref ()
    "Insert a ref or goto-link to a file."
    (interactive)
    (insert howm-ref-header
            " "
            (abbreviate-file-name (read-file-name "Insert ref to: " (concat howm-directory "data/")))))

  (defun my-howm-collect-keywords ()
    "Write to `howm-keyword-file' all keywords found in the current buffer."
    (interactive)
    (let* ((all-keywords)
           (org-regexp (concat "\\(?:"
                               "#\\+title: \\(.+\\)$"
                               "\\|"
                               "#\\+filetags: \\(.+\\)$"
                               "\\|"
                               "^keywords: \\(.+\\)$"
                               "\\)"))
           (text-regexp (concat "\\(?:"
                                "^keywords: \\(.+\\)$"
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

  (defun my-howm-list-grep-tag ()
    ;; based on howm-list-grep-general
    "Search notes using tags (@strings)."
    (interactive)
    (howm-set-command 'my-howm-list-grep-tag)
    (let* ((tag-regexp "^@")
           (regexp (howm-completing-read-keyword
                    (lambda (a) (string-match tag-regexp (car a))))))
      (howm-write-history regexp)
      (howm-search regexp t))))

;;; howm-org

(use-package howm-org
  :disabled      ; I don't like using org-style timestamps within howm
  :after (howm)
  :ensure nil)

;;; howm-reminder

(use-package howm-reminder
  :after (howm)
  :ensure nil
  :bind (("C-z 1" . howm-list-schedule)
         ("C-z 2" . my-howm-list-todo))
  :config
  ;; include timestamp part and note filenames
  (setopt howm-highlight-date-regexp-format "\\(?:\\[%Y-%m-%d.*?]\\|%Y-%m-%d\\)?")

  (defun my-howm-list-todo ()
    (interactive)
    (howm-list-todo)
    (howm-reminder-goto-today)))

;;; howm-riffle (riffle)

(use-package riffle ;; howm-riffle
  :after (howm)
  :ensure nil
  :custom
  (howm-view-close-frame/tab-on-exit t)
  (howm-view-search-recenter 5)
  (howm-view-keep-one-window t)
  (howm-view-split-horizontally nil)
  (howm-view-summary-window-size 30)
  (howm-view-window-location nil))

;;; howm-shift

(use-package howm-shift
  :after (howm)
  :ensure nil
  :bind (:map
         howm-mode-map
         ("S-<down>" . howm-shift-down)
         ("S-<left>" . howm-shift-left)
         ("S-<right>" . howm-shift-right)
         ("S-<up>" . howm-shift-up)))

;;; howm-vars

(use-package howm-vars
  :after (howm)
  :ensure nil
  :custom
  (howm-action-lock-forward-save-buffer t)
  (howm-check-word-break "^[[:alnum:]]+$")
  (howm-content-from-region t)
  (howm-directory "~/howm/")
  (howm-history-file (expand-file-name ".howm-history" howm-directory))
  (howm-history-limit nil)
  (howm-iigrep-preview-items 50)
  (howm-iigrep-show-what 'counts)
  (howm-keyword-case-fold-search t)
  (howm-keyword-file (expand-file-name ".howm-keys" howm-directory))
  (howm-list-recent-days 14)
  (howm-list-title-regexp "^(\\*$|(\\*|#\\+title:) +)") ; passed to grep/rg
  (howm-menu-file (expand-file-name "howm-menu.org" user-emacs-directory))
  (howm-menu-footer "")
  (howm-menu-name-format "*howm-menu:%s*")
  (howm-menu-recent-num 20)
  (howm-menu-schedule-days 30)
  (howm-menu-schedule-days-before 14)
  (howm-menu-todo-num 50)
  (howm-menu-todo-priority-format nil)
  (howm-message-time nil)
  (howm-normalizer 'howm-sort-items-by-mtime)
  (howm-prepend t)
  (howm-remember-first-line-to-title t)
  (howm-remember-insertion-format "%s")
  (howm-user-font-lock-keywords '(("^keywords:" . (0 'howm-mode-ref-face))))
  (howm-view-contents-name "*howm-contents*")
  (howm-view-contents-persistent nil)
  (howm-view-summary-name "*howm-summary*")
  (howm-view-summary-persistent nil)

  ;; Use rg/ripgrep for searching
  (howm-view-use-grep t)
  (howm-view-grep-command "rg")
  (howm-view-grep-expr-option "-e")
  (howm-view-grep-extended-option nil)
  (howm-view-grep-file-stdin-option "-f -")
  (howm-view-grep-fixed-option "-F")
  (howm-view-grep-option "-nH --no-heading --color never -g !data/")

  :preface
  (setq howm-excluded-dirs '("data" "RCS" "CVS" ".svn" ".git" "_darcs")
        howm-prefix nil)
  :config
  (setopt howm-file-name-format "%Y/%m/%Y-%m-%d.org")
  (add-to-list 'howm-list-title 'my-howm-list-grep-tag)
  (add-to-list 'howm-view-open-by-myself "image/.*"))

;;; howm-view

(use-package howm-view
  :after (howm howm-vars)
  :ensure nil
  :custom
  (howm-view-header-format "\n\n\n      [%s]\n\n")
  (howm-view-header-regexp nil)
  (howm-entitle-items-style2-format "%-50.50s | %s")
  (howm-entitle-items-style2-max-length 50)
  (howm-entitle-items-style2-title-line nil)
  (howm-view-list-title-type 1)
  :bind (:map
         howm-view-summary-mode-map
         ("<backtab>" . howm-view-summary-previous-section)
         ("M-n" . howm-view-summary-next-section)
         ("M-p" . howm-view-summary-previous-section)
         :map
         howm-view-contents-mode-map
         ("C-c C-b" . outline-backward-same-level)
         ("C-c C-f" . outline-forward-same-level)
         ("C-c C-n" . outline-next-heading)
         ("C-c C-p" . outline-previous-heading)
         ("M-n" . riffle-contents-goto-next-item)
         ("M-p" . riffle-contents-goto-previous-item)
         ;; unset both for `outline-minor-mode-cycle'
         ("<tab>" . nil)
         ("TAB" . nil))
  :hook ((howm-view-contents-mode-hook . hl-line-mode)
         (howm-view-contents-mode-hook . howm-mode)
         (howm-view-contents-mode-hook . howm-org-font-lock-minor-mode)
         (howm-view-contents-mode-hook . outline-minor-mode)
         (howm-view-contents-mode-hook . my-howm-other-modes-keys)
         (howm-view-summary-mode-hook . hl-line-mode)
         (howm-view-summary-mode-hook . my-howm-other-modes-keys))
  :preface
  (setq howm-view-title-header "*")
  :init
  (setq *howm-show-item-filename* nil ; don't show filenames in the echo area when browsing notes
        howm-view-summary-format (let* ((path (format-time-string howm-file-name-format))
                                        (width (length (file-name-nondirectory path)))
                                        (max-width (int-to-string 20)))
                                   (concat "%-" max-width "." max-width "s" howm-view-summary-sep " ")))

  (defun my-howm-other-modes-keys ()
    (mapc (lambda (map)
            (keymap-set map "." #'howm-reminder-goto-today)
            (keymap-set map "1" #'howm-list-schedule)
            (keymap-set map "2" #'howm-list-todo)
            (keymap-set map "M" #'howm-open-named-file)
            (keymap-set map "N" #'action-lock-goto-next-link)
            (keymap-set map "O" #'my-howm-create-other-window)
            (keymap-set map "P" #'action-lock-goto-previous-link)
            (keymap-set map "Q" #'howm-kill-all)
            (keymap-set map "W" #'howm-keyword-to-kill-ring)
            (keymap-set map "a" #'howm-list-all)
            (keymap-set map "c" #'howm-create)
            (keymap-set map "e" #'howm-remember)
            (keymap-set map "g" #'howm-list-grep)
            (keymap-set map "m" #'howm-menu)
            (keymap-set map "o" #'howm-occur)
            (keymap-set map "s" #'howm-list-grep-fixed)
            (keymap-set map "x" #'howm-list-mark-ring)
            (keymap-set map "M-h" #'outline-mark-subtree))
          (list howm-view-summary-mode-map
                howm-view-contents-mode-map)))
  :config
  ;; Cannot put in :custom due to howm-if-ver1dot3 check in howm code
  (setopt howm-view-title-skip-regexp
          (rx (or
               ;; Default/empty header line
               (group
                bol
                (opt
                 (eval howm-view-title-header)
                 (or " Notes" ""))
                (* " ")
                eol)

               ;; Line that starts with a timestamp (excluding todo items)
               (group
                bol
                "[" (+ (any "-" ":" " " digit)) "]"
                (or " " eol))

               ;; Keywords line
               (group
                bol
                "keywords:"
                (1+ (* space) "@" (+ any)))))))


;;; htmlize

(use-package htmlize)


;;; ibuffer

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer))
  :hook ((ibuffer-mode-hook . (lambda ()
                                (ibuffer-switch-to-saved-filter-groups "default")))
         (ibuffer-mode-hook . hl-line-mode))
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
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
                  (mode . emms-show-all-mode)))))))


;;; icomplete

(use-package icomplete
  :demand t
  :ensure nil
  :after (orderless)
  :custom
  (icomplete-prospects-height 1)
  (icomplete-separator (propertize "  |  " 'face 'font-lock-variable-name-face))
  (icomplete-compute-delay 0)
  (completion-auto-help t)
  (completion-cycle-threshold nil)
  (completion-pcm-complete-word-inserts-delimiters t)

  :init
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

  (defun my-icomplete-config ()
    (setq-local max-mini-window-height 0.15
                ;; Setting the completion-styles here is necessary
                ;; because `icomplete--fido-mode-setup' sets it to
                ;; flex by force.
                completion-styles '(orderless basic)

                ;; Completion falls back to using completion-styles if
                ;; completion-category-overrides doesn't yield a
                ;; result
                completion-category-overrides '((buffer
                                                 (styles . (basic flex partial-completion)))
                                                (file
                                                 (cycle-sort-function . minibuffer-sort-by-history)
                                                 (styles . (basic flex partial-completion)))
                                                (project-file
                                                 (cycle-sort-function . minibuffer-sort-by-history))))
    (keymap-set icomplete-minibuffer-map "C-?" #'minibuffer-hide-completions)
    (keymap-set icomplete-minibuffer-map "C-S-j" #'icomplete-force-complete)
    (keymap-set icomplete-minibuffer-map "C-<return>" #'icomplete-force-complete)
    (keymap-set icomplete-minibuffer-map "C-^" (lambda ()
                                                 (interactive)
                                                 (setq-local max-mini-window-height
                                                             (or (car (let ((window-sizes '(0.3 0.8)))
                                                                        (cl-remove-if (lambda (h)
                                                                                        (>= max-mini-window-height h))
                                                                                      window-sizes)))
                                                                 max-mini-window-height))))
    (keymap-set icomplete-minibuffer-map "C-c M-w" #'minibuffer-selection-kill-ring-save)
    (keymap-set icomplete-minibuffer-map "C-n" #'icomplete-forward-completions)
    (keymap-set icomplete-minibuffer-map "C-p" #'icomplete-backward-completions)
    (keymap-set icomplete-minibuffer-map "S-SPC" (lambda ()
                                                   (interactive)
                                                   (self-insert-command 1 ? )))
    (keymap-set icomplete-minibuffer-map "SPC" #'space-dash-star))

  (cond
   ((< emacs-major-version 27)
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
    (icomplete-mode t))

   ((= emacs-major-version 27)
    (fido-mode t)
    (add-hook 'icomplete-minibuffer-setup-hook #'my-icomplete-config))

   (t
    ;; Emacs 28 introduces `fido-vertical-mode'
    (fido-vertical-mode t)
    (add-hook 'icomplete-minibuffer-setup-hook #'my-icomplete-config))))


;;; image-dired

(require 'image-dired)
(use-package image-dired
  :ensure nil
  :after (dired)
  :bind (:map dired-mode-map
              ("C-t ." . image-dired-current-directory)
              ("C-t 0" . image-dired-standard-thumbnails)
              ("C-t 1" . image-dired-large-thumbnails)
              ("C-t 2" . image-dired-x-large-thumbnails)
              ("C-t 3" . image-dired-xx-large-thumbnails)
              :map image-dired-thumbnail-mode-map
              ("<tab>" . image-dired-forward-image)
              ("<backtab>" . image-dired-backward-image)
              ("f" . image-dired-forward-image)
              ("b" . image-dired-backward-image)
              ("n" . image-dired-next-line)
              ("p" . image-dired-previous-line))
  :config
  (defun image-dired-current-directory ()
    (interactive)
    (image-dired dired-directory))
  (defun image-dired-standard-thumbnails ()
    (interactive)
    (setopt image-dired-thumbnail-storage 'standard))
  (defun image-dired-large-thumbnails ()
    (interactive)
    (setopt image-dired-thumbnail-storage 'standard-large))
  (defun image-dired-x-large-thumbnails ()
    (interactive)
    (setopt image-dired-thumbnail-storage 'standard-x-large))
  (defun image-dired-xx-large-thumbnails ()
    (interactive)
    (setopt image-dired-thumbnail-storage 'standard-xx-large))
  :custom
  (image-dired-marking-shows-next nil)
  (image-dired-thumb-marin 10)
  (image-dired-thumb-relief 1)
  (image-dired-thumbnail-storage 'standard))


;;; imenu

(use-package imenu
  :ensure nil
  :hook ((emacs-lisp-mode-hook . my-imenu-emacs-lisp-mode)
         (restclient-mode-hook . my-imenu-restclient-mode))
  :config
  (defun my-imenu-emacs-lisp-mode ()
    (add-to-list 'imenu-generic-expression '("Sections" "^;;; \\(.*$\\)" 1))
    (add-to-list 'imenu-generic-expression '("Subsections" "^;;;; \\(.*$\\)" 1)))
  (defun my-imenu-restclient-mode ()
    (setq imenu-generic-expression '((nil "^#+ *\\(.+\\)\n[A-Z]+ " 1)
                                     ("Verb DELETE" "^\\(DELETE \\).*$" 0)
                                     ("Verb PUT" "^\\(PUT \\).*$" 0)
                                     ("Verb POST" "^\\(POST\\).*$" 0)
                                     ("Verb GET" "^\\(GET \\).*$" 0)
                                     ("Sections" "^\n#+ *\\(.*\\)\n$" 1)))))


;;; isearch

(use-package isearch
  :ensure nil
  ;; :bind (:map isearch-mode-map
  ;;             ("C-g" . my-isearch-control-g)
  ;;             ("C-`" . other-window))
  :custom
  (isearch-allow-scroll 'unlimited)
  (isearch-lazy-count t)
  (search-whitespace-regexp "[ \t\r\n]+")
  :config
  (defun my-isearch-control-g ()
    "Skip the rub out behavior and cancel the current isearch."
    (interactive)
    (setq isearch-success nil)
    (isearch-cancel)))


;;; js

;; Installed packages for related to Javascript development:
;; tide/tsserver for LSP/eglot

(use-package js
  :ensure nil
  :hook ((js-mode-hook . eglot-ensure)
         (js-mode-hook . subword-mode))
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2))


;;; json-navigator

(use-package json-navigator
  :bind (:map json-navigator-mode-map
              ("*" . tree-mode-expand-level)
              ("SPC" . tree-mode-toggle-expand)
              ("^" . tree-mode-goto-parent)
              ("n" . widget-forward)
              ("p" . widget-backward))
  :custom
  (json-navigator-display-length 10)
  :config
  (require 'tree-mode))


;;; json-ts-mode

(use-package json-ts-mode
  :ensure nil
  :bind (:map
         json-ts-mode-map
         ("C-c C-f" . my-json-pretty-print)
         ("C-c C-p" . my-json-path-to-position))
  :hook (no-indent-tabs-mode set-indent-tab-width-2)
  :mode (("\\.json\\'" . json-ts-mode))
  :config
  (require 'json)
  (defun my-json-pretty-print ()
    (interactive)
    (if (region-active-p)
        (json-pretty-print-ordered (region-beginning) (region-end))
      (json-pretty-print-buffer-ordered)))

  (defun my-json-path-to-position ()
    (interactive)
    (message "%s" (json-path-to-position (point)))))


;;; jsonian

(use-package jsonian
  :disabled)


;;; know-your-http-well

(use-package know-your-http-well)


;;; llm-tool-collection

(use-package llm-tool-collection
  :after (gptel)
  :demand t
  :ensure (:repo "~/.emacs.d/packages/llm-tool-collection")
  :config
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-category "filesystem"))
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-category "buffers")))


;;; localsend

(use-package localsend
  :disabled
  :ensure nil
  :config
  (require 'localsend-scratch))


;;; lorem-ipsum

(use-package lorem-ipsum)


;;; magit

(use-package magit
  :bind (("C-c g" . magit-dispatch)
         ("C-x g" . magit-status))
  ;; :hook ((magit-status-sections-hook . magit-insert-modules))
  :custom
  (magit-define-global-key-bindings nil)
  (magit-diff-refine-hunk t)
  (magit-status-goto-file-position t)
  (magit-status-show-hashes-in-headers t)
  :config
  (defun magit-commit-create-updates ()
    (interactive)
    (magit-commit-create '("-m" "updates")))
  (transient-append-suffix 'magit-commit "c"
    '("C" "Commit generic" magit-commit-create-updates)))


;;; magit-todos

(use-package magit-todos
  :after (magit)
  :demand t
  :custom
  (magit-todos-branch-list nil) ;; fixes perf issues on large files added in repo
  (magit-todos-exclude-globs '(".git/" "node_modules/" "*.json"))
  :config
  (magit-todos-mode 1))


;;; marginalia

(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
              ("M-M" . marginalia-cycle))
  :config
  (marginalia-mode))


;;; markdown-mode

(use-package markdown-mode
  :hook ((markdown-mode-hook . my-markdown-mode-config))
  :bind (:map
         markdown-mode-map
         ("C-<return>" . markdown-insert-header-dwim)
         ("M-<left>" . markdown-promote)
         ("M-<right>" . markdown-demote))
  :custom
  (markdown-asymmetric-header t)
  (markdown-make-gfm-checkboxes-buttons nil)
  :config
  (defun my-markdown-mode-config ()
    (indent-tabs-mode -1)
    (visual-line-fill-column-mode t)
    (setq fill-column 100
          markdown-unordered-list-item-prefix "  * ")))


;;; mcp

(use-package mcp
  :after (gptel)
  :bind (:map
         my-meta-=-map
         ("m" . mcp-hub))
  :custom
  (mcp-hub-servers
   `(
     ;; ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ("filesystem" . (:command "npx"
                               :args ("-y" "@modelcontextprotocol/server-filesystem")
                               :roots ("~/mcp-filesystem/")))
     ("org-mcp" . (:command "uvx"
                            :args ("org-mcp")
                            :env (:ORG_DIR ,(expand-file-name howm-directory))))
     ("mcp-nixos" . (:command "uvx"
                              :args ("mcp-nixos"))))))


;;; mermaid-mode

(use-package mermaid-mode
  :bind (:map
         mermaid-mode-map
         ("C-c C-b" . mermaid-compile-buffer)
         ("C-c C-d" . mermaid-open-doc)
         ("C-c C-f" . mermaid-compile-file)
         ("C-c C-o" . mermaid-open-browser)
         ("C-c C-r" . mermaid-compile-region)
         ("C-c C-c" . (lambda ()
                        (interactive)
                        (save-buffer)
                        (mermaid-compile)))
         ("C-c C-s" . my-mermaid-mode-compile-svg))
  :hook ((mermaid-mode-hook . my-mermaid-mode-config))
  :mode (("\\.mermaid\\'" . mermaid-mode)
         ("\\.mmd\\'" . mermaid-mode))
  :custom
  (mermaid-output-format ".png")
  (mermaid-flags "-s 3")
  :config
  (defun my-mermaid-mode-config ()
    (setq-local indent-tabs-mode nil))

  (defun my-mermaid-mode-compile-svg ()
    (interactive)
    (save-buffer)
    (let* ((mermaid-output-format ".svg")
           (input (buffer-file-name))
           (output (concat (file-name-sans-extension input) mermaid-output-format))
           (exit-code (apply #'call-process
                             mermaid-mmdc-location nil "*mmdc*" nil (append (split-string mermaid-flags " ")
                                                                            (list "-i" input "-o" output)))))
      (when (zerop exit-code)
        (cond ((eq system-type 'darwin)
               (shell-command (concat "open " (shell-quote-argument output))))
              ((eq system-type 'gnu/linux)
               (find-file-other-window (shell-quote-argument output))))
        ))))


;;; mermaid-ts-mode

(use-package mermaid-ts-mode
  :disabled
  :after (mermaid-mode)
  :custom
  (mermaid-ts-indent-level 4))


;;; misearch

(use-package misearch
  :ensure nil
  :bind (:map my-ctl-c-d-map
              ("M-%" . replace-regexp-as-diff)))


;;; mode-local

(use-package mode-local
  :demand t
  :ensure nil)


;;; multi-term

(use-package multi-term
  :bind (:map my-ctl-c-t-map
              ("T" . multi-term)
              ("t" . multi-term-next))
  :custom
  (multi-term-program "/run/current-system/sw/bin/bash")
  (term-bind-key-alist '(("C-c C-c" . term-interrupt-subjob)
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
                         ("M-." . comint-dynamic-complete))))


;;; multiple-cursors

(use-package multiple-cursors
  :bind (:map my-ctl-c-m-map
              ("c ." . mc/mark-all-like-this-dwim)
              ("c C-M-SPC" . mc/mark-all-in-region-regexp)
              ("c C-SPC" . mc/mark-all-in-region)
              ("c C-a" . mc/edit-beginnings-of-lines)
              ("c C-e" . mc/edit-ends-of-lines)
              ("c C-n" . mc/mark-next-like-this)
              ("c C-p" . mc/mark-previous-like-this)
              ("c e" . mc/edit-lines)
              ("c i l" . mc/insert-letters)
              ("c i n" . mc/insert-numbers)
              :map mc/keymap
              ("<return>" . nil)
              ("C-c ," . mc/mark-previous-like-this)
              ("C-c ." . mc/mark-next-like-this)
              ("C-c <" . mc/skip-to-previous-like-this)
              ("C-c >" . mc/skip-to-next-like-this)))


;;; nix-ts-mode

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook ((nix-ts-mode-hook . eglot-ensure))
  :config
  (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))


;;; nixfmt

(use-package nixfmt
  :hook ((nix-ts-mode-hook . nixfmt-on-save-mode)))


;;; nov

(use-package nov
  :bind (:map nov-mode-map
              ("w" . my-nov-update-text-width))
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode-hook . (lambda () (face-remap-add-relative 'default :height 1.1))))
  :custom
  (nov-text-width 80)
  :config
  (defun my-nov-update-text-width (n)
    "Change `nov-text-width' to width N."
    (interactive (list (or current-prefix-arg
                           (read-number (format "Change nov-text-width from %s to: " nov-text-width)
                                        (current-column)))))
    (setq-local nov-text-width n)
    (nov-render-document)))


;;; nxml

(use-package nxml-mode
  :ensure nil
  :custom
  (nxml-child-indent 4))

(use-package nxml-outln
  :after (nxml-mode)
  :ensure nil
  :custom
  (nxml-outline-child-indent 4))


;;; ob-chatgpt-shell

(use-package ob-chatgpt-shell
  :config
  (ob-chatgpt-shell-setup))


;;; orderless

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic)))


;;; org

(use-package org
  :demand t
  :ensure nil
  :bind (;; ("C-c C" . org-capture)
         ;; :map
         ;; my-ctl-c-o-map
         ;; ("a" . org-agenda)
         ;; ("b" . org-switchb)
         :map
         org-mode-map
         ("C-c C--" . org-ctrl-c-minus)
         ("C-c C-8" . org-ctrl-c-star)
         ("C-c C-SPC" . org-table-blank-field)
         ("C-M-q" . my-org-fixup-whitespace)
         ("C-M-h" . org-mark-subtree)
         ("C-," . nil))
  :hook ((org-mode-hook . no-indent-tabs-mode)
         (org-mode-hook . (lambda ()
                            (set-fill-column 100)))
         (org-mode-hook . visual-line-fill-column-mode))
  :custom
  (org-adapt-indentation nil)
  (org-agenda-files (expand-file-name "org-agenda-files" user-emacs-directory))
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-archive-location "archive/%s::")
  (org-auto-align-tags nil)
  (org-complete-tags-always-offer-all-agenda-tags t)
  (org-default-notes-file "~/org/inbox.org")
  (org-fontify-done-headline nil)
  (org-fontify-todo-headline nil)
  (org-hide-emphasis-markers nil)
  (org-hide-leading-stars nil)
  (org-image-actual-width 500)
  (org-log-into-drawer t)
  (org-reverse-note-order t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-src-fontify-natively nil)
  (org-startup-indented nil)
  (org-startup-with-inline-images t)
  (org-tags-column 0)
  (org-use-fast-todo-selection 'expert)
  (org-use-sub-superscripts '{})
  (org-todo-keywords '((sequence "TODO(t)" "WIP(p)" "DEFERRED(f@)" "WAITING(w@)"
                                 "|" "DONE(d@/@)" "CANCELED(c@/@)")
                       (type "INBOX(i)" "TOPIC(o)")))
  :config
  (defun my-org-fixup-whitespace ()
    "Fix Org document indentation, and tag alignment if universal arg was given."
    (interactive)
    (if (region-active-p)
        (org-indent-region (region-beginning) (region-end))
      (org-indent-region (point-min) (point-max)))
    ;; Call with a C-u prefix to fixup tag indentation
    (let ((current-prefix-arg '(4)))
      (call-interactively #'org-set-tags-command)))

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
      (org-id-get-create))))

;;; org-agenda

(use-package org-agenda
  :disabled
  :after (org)
  :ensure nil
  :hook (;; (org-mode-hook . my-org-update-org-agenda-on-save)
         (org-agenda-mode-hook . hl-line-mode))
  :custom
  (org-agenda-category-icon-alist '())
  (org-agenda-search-view-max-outline-level 2)
  (org-agenda-show-outline-path nil)
  (org-agenda-start-with-follow-mode nil)
  (org-agenda-custom-commands
   '(("I" "Ideate TODOs" tags-todo "ideate")
     ("P" "All TODOs" ((tags-todo "@project-CATEGORY=\"routines\"") (tags-todo "@inbox")))
     ("R" "All routines" ((tags-todo "+CATEGORY=\"routines\"")))))
  :config
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
    (add-hook 'before-save-hook #'my-org-update-org-agenda-files nil t)))

;;; org-archive

(use-package org-archive
  :after (org)
  :ensure nil
  :custom
  (org-archive-file-header-format "#+filetags: :ARCHIVE:\n\n")
  (org-archive-reversed-order t))

;;; org-attach

(use-package org-attach
  :after (org)
  :bind (:map
         org-mode-map
         ("C-c C-a" . my-org-attach))
  :ensure nil
  :custom
  (org-attach-expert nil)
  (org-attach-id-dir "~/org/data/") ; To allow viewing attachments even when archived
  (org-attach-method 'mv)
  (org-attach-preferred-new-method 'dir)
  (org-attach-store-link-p 'attached)
  :config
  (defun my-org-attach ()
    "Use Downloads as the default target directory."
    (interactive)
    (let ((dired-dwim-target (lambda ()
                               (list "~/Downloads/"))))
      (call-interactively #'org-attach)))

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
  (advice-add 'org-attach-attach :before #'my-add-org-property-dir))

;;; org-capture

(use-package org-capture
  :after (org org-refile)
  :ensure nil
  ;; Hook `beginning-of-buffer' so the ID property gets added to the root node
  :hook ((org-capture-prepare-finalize-hook . beginning-of-buffer)
         (org-capture-prepare-finalize-hook . whitespace-cleanup)
         (org-capture-prepare-finalize-hook . single-blank-lines-only)
         (org-capture-before-finalize-hook . my-org-id-get-create))
  :bind (:map
         org-capture-mode-map
         ("C-c C-M-t" . my-org-capture-refile-to-topic))
  :custom
  (org-capture-templates
   '(("c" "Capture something" entry
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
      :prepend t)))

  :config
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

  (defun my-org-capture-refile-to-topic ()
    "Limit `org-refile-targets' to those having todo keyword TOPIC in the current file plus the inbox."
    (interactive)
    (let ((org-refile-use-outline-path t)
          (org-refile-targets '((nil :todo . "TOPIC")
                                ("~/org/inbox.org" :todo . "TOPIC"))))
      (org-capture-refile))))

;;; org-cycle

(use-package org-cycle
  :after (org)
  :ensure nil
  :custom
  (org-cycle-inline-images-display t))

;;; org-element

(use-package org-element
  :after (org)
  :demand t ;; For howm-org-font-lock-minor-mode
  :ensure nil
  :bind (:map
         org-mode-map
         ("C-c o L" . my-org-link-retain-description))
  :config
  (defun my-org-link-retain-description ()
    "Delete an org-mode-link and retain only the description."
    (interactive)
    (let* ((element (org-element-context))
           (element-type (car element)))
      (when (eq 'link element-type)
        (let* ((contents (buffer-substring (org-element-property :contents-begin element)
                                           (org-element-property :contents-end element))))
          (delete-region (org-element-property :begin element)
                         (org-element-property :end element))
          (insert contents))))))

;;; org-faces

(use-package org-faces
  :after (org)
  :ensure nil
  :custom
  (org-todo-keyword-faces
   '(("CANCELED" :foreground "black" :background "gainsboro" :box (:style released-button) :height 1.0)
     ("DEFERRED" :foreground "black" :background "azure1" :box (:style released-button) :height 1.0)
     ("DONE" :foreground "black" :background "honeydew1" :box (:style released-button) :height 1.0)
     ("INBOX" :foreground "black" :background "lightskyblue1" :box (:style released-button) :height 1.0)
     ("TODO" :foreground "black" :background "mistyrose1" :box (:style released-button) :height 1.0)
     ("TOPIC" :foreground "black" :background "slategray1" :box (:style released-button) :height 1.0)
     ("WAITING" :foreground "black" :background "plum1" :box (:style released-button) :height 1.0)
     ("WIP" :foreground "black" :background "peachpuff1" :box (:style released-button) :height 1.0))))

;;; org-indent

(use-package org-indent
  :after (org)
  :ensure nil
  ;; :hook ((org-mode-hook . org-indent-mode))
  :custom
  (org-indent-mode-turns-on-hiding-stars t))

;;; org-link (ol)

(use-package ol
  :after (org)
  :ensure nil
  :bind (:map
         my-ctl-c-o-map
         ("s" . org-store-link))
  :custom
  (org-link-descriptive t))

;;; org-refile

(use-package org-refile
  :after (org)
  :ensure nil
  :hook ((org-after-refile-insert-hook . my-org-id-get-create))
  :bind (:map
         org-mode-map
         ("C-c C-M-t" . my-org-refile-to-topic))
  :custom
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((nil :maxlevel . 2)
                        ;; ("~/org/inbox.org" :todo . "TOPIC")
                        ;; (my-org-refile-target-projects :todo . "INBOX")
                        ;; (my-org-refile-target-projects :todo . "TODO")
                        (my-org-refile-target-howm-named-notes :maxlevel . 1)))
  :config
  (defun my-org-refile-target-projects ()
    (directory-files "~/org/projects" t directory-files-no-dot-files-regexp))

  (defun my-org-refile-target-howm-named-notes ()
    (directory-files howm-directory t ".*\\.org"))

  (defun my-org-refile-to-topic ()
    "Limit `org-refile-targets' to those having todo keyword TOPIC in the current file plus the inbox."
    (interactive)
    (let ((org-refile-use-outline-path t)
          (org-refile-targets '((nil :todo . "TOPIC")
                                ("~/org/inbox.org" :todo . "TOPIC"))))
      (org-refile))))

;;; org-src

(use-package org-src
  :after (org)
  :ensure nil
  :custom
  (org-edit-src-content-indentation 0)
  (org-src-window-setup 'current-window))


;;; org-alert

(use-package org-alert
  :disabled
  :after (alert)
  :custom
  (org-alert-interval 300)
  (org-alert-notification-title "Org alert")
  (org-alert-notify-cutoff 0)
  (org-alert-notify-after-event-cutoff 10)
  :init
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
  :config
  (org-alert-enable))


;;; org-babel (ob)

(use-package ob
  :after (org)
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((calc . t)
     (emacs-lisp . t)
     (js . t)
     (latex . t)
     (plantuml . t)
     (python . t)
     (shell . t))))


;;; org-bookmark-heading

(use-package org-bookmark-heading
  :after (org)
  :demand t)


;;; org-crypt

(use-package org-crypt
  :ensure nil
  :after (org)
  :bind (:map
         org-mode-map
         ("C-c o e" . org-encrypt-entry)
         ("C-c o d" . org-decrypt-entry)
         ("C-c o E" . org-encrypt-entries)
         ("C-c o D" . org-decrypt-entries))
  :init
  (org-crypt-use-before-save-magic)
  (add-to-list 'org-tags-exclude-from-inheritance "crypt")
  :custom
  (org-crypt-key "0x837541986DF6E7AB")
  (org-crypt-disable-auto-save t))


;;; org-modern

(use-package org-modern
  :disabled
  :after (org)
  :bind (:map
         org-mode-map
         ("C-c o m" . org-modern-mode))
  :hook ((org-mode-hook . org-modern-mode))
  :custom
  (org-modern-block-name nil)
  (org-modern-block-fringe nil)
  (org-modern-checkbox nil)
  (org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("▶" . "▼") ("▷" . "▽") ("▶" . "▼")))
  (org-modern-hide-stars nil)
  (org-modern-keyword nil)
  (org-modern-list nil)
  (org-modern-priority t)
  (org-modern-priority-faces '((?A :background "whitesmoke" :foreground "hotpink" :weight bold :box (:style released-button :line-width (0 . -1)))
                               (?B :background "whitesmoke" :foreground "cadetblue" :weight bold :box (:style released-button :line-width (0 . -1)))
                               (?C :background "whitesmoke" :foreground "gray" :weight bold :box (:style released-button :line-width (0 . -1)))))
  (org-modern-progress nil)
  (org-modern-radio-target '("「" t "」"))
  (org-modern-replace-stars "■▪■▪■▪")
  (org-modern-star nil)
  (org-modern-tag t)
  (org-modern-tag-faces '((t :background "beige" :foreground "black" :weight normal :box (:style pressed-button :line-width (0 . -1)))))
  (org-modern-timestamp nil)
  (org-modern-todo nil)
  (org-modern-todo-faces '(("CANCELED" :background "gainsboro" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                           ("DEFERRED" :background "azure1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                           ("DONE" :background "honeydew1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                           ("INBOX" :background "lightskyblue1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                           ("TODO" :background "mistyrose1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                           ("TOPIC" :background "slategray1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                           ("WAITING" :background "plum1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1)))
                           ("WIP" :background "peachpuff1" :foreground "black" :weight normal :box (:style released-button :line-width (0 . -1))))))


;;; org-present

(use-package org-present
  :after (org)
  :hook ((org-present-mode-hook . (lambda ()
                                    (org-present-big)
                                    (org-display-inline-images)
                                    (org-present-hide-cursor)
                                    (read-only-mode)))
         (org-present-quit-hook . (lambda ()
                                    (org-present-small)
                                    (org-remove-inline-images)
                                    (org-present-show-cursor)
                                    (read-only-mode -1)))))


;;; org-roam

(use-package org-roam
  :disabled
  :after (org)
  :custom
  (org-roam-directory "~/org")
  (org-roam-db-location "~/org/org-roam.db"))

;;; org-roam-node

(use-package org-roam-node
  :ensure nil
  :after (org-roam)
  :bind (:map
         my-ctl-c-n-map
         ("%" . org-roam-node-random)
         ("," . org-roam-dailies-goto-previous-note)
         ("." . org-roam-dailies-goto-next-note)
         ("C" . org-roam-capture)
         ("J" . org-roam-dailies-capture-date)
         ("c" . my-org-roam-capture)
         ("f" . org-roam-node-find)
         ("j" . org-roam-dailies-capture-today)
         ("y" . org-roam-dailies-goto-yesterday)
         :map
         org-mode-map
         ("C-c n A" . org-roam-alias-remove)
         ("C-c n I" . org-roam-node-insert-immediate-finish)
         ("C-c n R" . org-roam-ref-remove)
         ("C-c n T" . org-roam-tag-remove)
         ("C-c n X" . my-org-roam-extract-subtree-inbox-entry)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . my-org-roam-add-link-to-region)
         ("C-c n n" . org-id-get-create)
         ("C-c n r" . org-roam-ref-add)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n x" . org-roam-extract-subtree)
         :map
         org-roam-node-map
         ("C-c n l" . org-roam-buffer-toggle))
  :hook ((org-roam-mode-hook . (lambda ()
                                 (visual-line-fill-column-mode)
                                 (set-fill-column org-roam-content-width))))
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template (concat
                                   "${title:100} "
                                   (propertize "${tags}" 'foreground 'default)
                                   "${myarchive-itags}"
                                   "${mytodo}"))
  :config
  (defvar org-roam-content-width 60)
  (add-to-list 'display-buffer-alist
               `("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . left)
                 (window-width . ,(+ 5 org-roam-content-width))
                 (window-height . fit-window-to-buffer)))

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
                       :props '(:immediate-finish nil))))

;;; org-roam-capture

(use-package org-roam-capture
  :ensure nil
  :after (org-roam)
  :custom
  (org-roam-capture-templates '(("d" "default" plain "%?"
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
                                 :unnarrowed t))))

;;; org-roam-dailies

(use-package org-roam-dailies
  :ensure nil
  :after (org-roam)
  :bind (:repeat-map
         org-roam-dailies-repeat-map
         ("." . org-roam-dailies-goto-next-note)
         ("," . org-roam-dailies-goto-previous-note))
  :custom
  (org-roam-dailies-capture-templates '(("d" "default" entry "* %?"
                                         :target (file+head "%<%Y-%m-%d>.org" "#+title: Journal - %<%Y-%m-%d>\n")
                                         :empty-lines 1))))


;;; org-sticky-header

(use-package org-sticky-header
  :after (org)
  :custom
  (org-sticky-header-at-point t)
  (org-sticky-header-full-path 'full)
  (org-sticky-header-heading-star "*"))


;;; org-web-tools

(use-package org-web-tools
  :after (org)
  :bind (:map
         org-mode-map
         ("C-c o C-y" . org-web-tools-insert-link-for-url)
         ("C-c o w a" . org-web-tools-archive-attach)
         ("C-c o w i" . org-web-tools-insert-web-page-as-entry)
         ("C-c o w v" . org-web-tools-archive-view)))


;;; orgalist

(use-package orgalist
  :bind (:map
         orgalist-mode-map
         ("C-c C--" . orgalist-cycle-bullet)
         ("M-<down>" . orgalist-move-item-down)
         ("M-<up>" . orgalist-move-item-up)
         ("M-S-<return>" . orgalist-insert-checkbox))
  :config
  (defun orgalist-insert-checkbox ()
    (interactive)
    (orgalist-insert-item t)))


;;; origami

(use-package origami
  :disabled
  :demand t
  :bind (:map
         origami-mode-map
         ("C-c f f" . origami-toggle-node)
         ("C-c f O" . origami-open-all-nodes)
         ("C-c f C" . origami-close-all-nodes))

  :config
  (global-origami-mode))


;;; osm

(use-package osm
  :custom
  (osm-home '(12.49 122.48 6)))


;;; outline

(use-package outline
  :ensure nil
  :custom
  (outline-default-state nil)
  (outline-minor-mode-cycle t))


;;; ox

(use-package ox
  :after (org)
  :ensure nil
  :custom
  (org-export-with-sub-superscripts '{}))


;;; ox-asciidoc

(use-package ox-asciidoc
  :after (ox))


;;; ox-gfm

(use-package ox-gfm
  :after (ox))


;;; ox-jira

(use-package ox-jira
  :after (ox)
  :demand t)


;;; ox-md

(use-package ox-md
  :after (ox)
  :demand t
  :ensure nil)


;;; ox-slack

(use-package ox-slack
  :disabled
  :after (ox))


;;; package-lint

(use-package package-lint)


;;; pass

(use-package pass
  :custom
  (pass-suppress-confirmations t)
  :bind (:map
         my-ctl-c-P-map
         ("P" . pass)))


;;; password-store

(use-package password-store)


;;; password-store-otp

(use-package password-store-otp
  :ensure (:protocol ssh :remotes (("fork" :repo "nicolaisingh/password-store-otp.el"))))


;;; pdf-tools

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install t))


;;; plantuml-mode

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :custom
  (plantuml-default-exec-mode 'executable)
  (plantuml-jar-path "~/.nix-profile/bin/plantuml")
  (plantuml-output-type "png"))


;;; prettier-js

(use-package prettier-js
  :diminish
  :hook ((js2-mode-hook . prettier-js-mode)
         (typescript-ts-mode-hook . prettier-js-mode)))


;;; prism

(use-package prism
  :after (personal-2-theme)
  :bind (:map my-ctl-c-h-map
              ("b" . prism-mode)
              ("w" . prism-whitespace-mode))
  :hook ((yaml-ts-mode-hook . prism-whitespace-mode))
  :custom
  (prism-comments nil)
  (prism-desaturations '(0))
  (prism-lightens '(0))
  (prism-whitespace-mode-indents '((python-mode . python-indent-offset)
                                   (haskell-mode . haskell-indentation-left-offset)
                                   (yaml-mode . yaml-indent-offset)
                                   (yaml-ts-mode . 2)
                                   (t . tab-width))))


;;; prodigy

(use-package prodigy
  :bind (:map my-ctl-c-P-map ("p" . prodigy))
  :config
  (prodigy-define-service
    :name "proton-bridge"
    :command "protonmail-bridge"
    :args '("-n")
    :tags '(proton))

  (prodigy-define-service
    :name "sample-project"
    :command "~/prj/sample/local_build.sh"
    :args '("")
    :cwd "~/prj/sample"
    :tags '(ctl-c))

  (prodigy-define-tag
    :name 'proton
    :stop-signal 'int
    :kill-process-buffer-on-stop t)

  (prodigy-define-tag
    :name 'ctl-c
    :stop-signal 'int
    :ready-message "Press CTRL\\+C to quit"))


;;; project

(use-package project
  :ensure nil
  :config
  ;; Add magit as m
  (keymap-set project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  ;; Add deadgrep as G
  (keymap-set project-prefix-map "G" #'deadgrep)
  (add-to-list 'project-switch-commands '(deadgrep "Deadgrep") t))


;;; pytest

(use-package pytest
  :after (python)
  :bind (:map
         python-mode-map
         ("C-c P ." . pytest-one)
         ("C-c P !" . pytest-again)
         ("C-c P m" . pytest-module)
         ("C-c P a" . pytest-all)
         ("C-c C ." . pytest-cov-one)
         ("C-c C !" . pytest-cov-again)
         ("C-c C m" . pytest-cov-module)
         ("C-c C a" . pytest-cov-all))
  :config
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
      (pytest-all))))


;;; python

(use-package python
  :ensure nil
  :bind (:map python-mode-map
              ("C-c C-b" . (lambda ()
                             "Create a buffer-dedicated python shell."
                             (interactive)
                             (run-python (python-shell-calculate-command) t t)))
              ("C-c C-t" . python-shell-restart))
  :custom
  (python-indent-def-block-scale 1)
  :hook ((python-mode-hook . subword-mode)
         (python-mode-hook . eglot-ensure))
  :config
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
          (message "Wrote config to %s" (concat out-dir out-filename)))))))


;;; pyvenv

(use-package pyvenv)


;;; qrencode

(use-package qrencode)


;;; rainbow-mode

(use-package rainbow-mode)


;;; recentf

(use-package recentf
  :demand t
  :ensure nil
  :custom
  (recentf-auto-cleanup 60)
  (recentf-max-saved-items 5000)
  (recentf-max-menu-items 5000)
  :config
  (add-to-list 'recentf-exclude "\\/sudoedit:root")
  (add-to-list 'recentf-exclude "~\\'")
  (add-to-list 'recentf-exclude "\\.el\\.gz\\'")
  (run-at-time nil (* 5 60) 'recentf-save-list) ; Save every 5 minutes

  (defun recentf-cleanup-quietly (orig-fun &rest _)
    "Don't print log messages while calling ORIG-FUN."
    (let ((inhibit-message t)) (funcall orig-fun)))
  (advice-add #'recentf-cleanup :around #'recentf-cleanup-quietly)

  (defun recentf-save-list-quietly (orig-fun &rest _)
    "Don't print log messages while calling ORIG-FUN."
    (let ((inhibit-message t)) (funcall orig-fun)))
  (advice-add #'recentf-save-list :around #'recentf-save-list-quietly)

  (recentf-mode))


;;; repeat

(use-package repeat
  :demand t
  :ensure nil
  :init
  (repeat-mode t))


;;; restclient

(use-package restclient
  :mode (("\\.restclient\\'" . restclient-mode)
         ("\\.http\\'" . restclient-mode))
  :hook ((restclient-mode-hook . no-indent-tabs-mode)
         (restclient-mode-hook . smartparens-mode)
         (restclient-mode-hook . set-indent-tab-width-2))
  :custom
  (restclient-same-buffer-response nil)
  (restclient-response-size-threshold nil))


;;; saveplace

(use-package saveplace
  :demand t
  :ensure nil
  :config
  (save-place-mode 1))


;;; saveplace-pdf-view

(use-package saveplace-pdf-view
  :after (:any doc-view pdf-tools)
  :demand t
  :ensure (:protocol ssh))


;;; scheme

(use-package scheme
  :ensure nil
  :bind (:map scheme-mode-map
              ("M-<return>" . scheme-send-last-sexp)
              ("s" . run-scheme))
  :hook ((scheme-mode-hook . no-indent-tabs-mode)))


;;; selected

(use-package selected
  :demand t
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("= p" . my-gptel-proofread)
              ("C" . capitalize-region)
              ("D" . delete-duplicate-lines)
              ("E" . flush-empty-lines)
              ("F" . flush-lines)
              ("K" . keep-lines)
              ("R" . replace-region-with)
              ("S" . my-sort-string-literals)
              ("SPC" . canonically-space-region)
              ("a" . align-regexp)
              ("f" . fill-region)
              ("l" . gptel-send)
              ("q" . selected-off)
              ("r" . reverse-region)
              ("s" . my-sort-lines)
              ("u" . unfill-region)
              ("w" . gptel-rewrite)
              ("C-c C-a" . mc/edit-beginnings-of-lines)
              ("C-c C-e" . mc/edit-ends-of-lines)
              ("C-c C-SPC" . mc/mark-all-in-region)
              ("C-c C-M-SPC" . mc/mark-all-in-region-regexp))
  :hook ((chatgpt-shell-prompt-compose-mode-hook . turn-off-selected-minor-mode)
         (magit-mode-hook . turn-off-selected-minor-mode)
         (multiple-cursors-mode-hook . my-mc-toggle-selected-minor-mode))
  :init
  (defun turn-off-selected-minor-mode ()
    (selected-minor-mode -1))

  (defun my-mc-toggle-selected-minor-mode ()
    (if multiple-cursors-mode
        (selected-minor-mode -1)
      (selected-minor-mode 1)))

  (defun my-sort-lines (reverse beg end)
    "Sort lines in region, excluding leading whitespaces, alphabetically."
    (interactive "P\nr")
    ;; Only sort non-whitespace chars
    (sort-regexp-fields reverse "^\\s-*\\(.+\\)$" "\\1" beg end))

  (defun my-sort-string-literals (reverse beg end)
    "Sort string literals in the region."
    (interactive "P\nr")
    (sort-regexp-fields reverse "\"[^\"]*\"" "\\&" beg end))

  (defun flush-empty-lines ()
    (interactive)
    (flush-lines "^$" (region-beginning) (region-end)))

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
  :config
  (selected-global-mode))


;;; sendmail

(use-package sendmail
  :ensure nil
  :custom
  (send-mail-function #'sendmail-send-it)
  (sendmail-program "msmtp"))


;;; slime

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))


;;; smartparens

(use-package smartparens
  :demand t
  :diminish smartparens-mode
  :bind (:map
         smartparens-mode-map
         ;; movement
         ("C-M-b" . sp-backward-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ;; barf/slurp
         ("C-(" . sp-backward-slurp-sexp)
         ("C-)" . my-sp-slurp-sexp)
         ("M-(" . sp-backward-barf-sexp)
         ("M-)" . sp-forward-barf-sexp)
         ;; editing
         ("M-C" . sp-change-enclosing)
         ("M-D" . sp-kill-symbol)
         ("M-J" . sp-join-sexp)
         ("M-R" . sp-raise-sexp)
         ("M-S" . sp-splice-sexp)
         ("M-S-<backspace>" . sp-backward-kill-symbol)
         ("M-U" . sp-unwrap-sexp)
         ("M-W" . sp-wrap-round)
         :map
         my-ctl-c-s-map
         ("C" . sp-convolute-sexp)
         ("n" . sp-narrow-to-sexp))
  :hook (prog-mode-hook text-mode-hook)
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-show-pair-delay 0)
  :init
  (require 'smartparens-config)
  (defun my-sp-slurp-sexp (&optional arg)
    (interactive "*P")
    (if (memq major-mode sp-lisp-modes)
        (sp-forward-slurp-sexp arg)
      (sp-slurp-hybrid-sexp))))


;;; smtpmail

(use-package smtpmail
  :ensure nil
  :custom
  ;; (smtpmail-servers-requiring-authorization "127.0.0.1")
  (smtpmail-smtp-server "127.0.0.1")
  (smtpmail-smtp-service 1025)
  (smtpmail-stream-type 'starttls))


;;; subword

(use-package subword
  :ensure nil
  :diminish subword-mode)


;;; tempel

(use-package tempel
  :disabled
  :demand t
  :bind (:map
         my-ctl-c-t-map
         ("c" . tempel-complete)
         ("i" . tempel-insert)))


;;; terraform-mode

(use-package terraform-mode)


;;; tex-mode

(use-package tex-mode
  :ensure nil
  :bind (:map
         latex-mode-map
         ("C-c C-f" . my-tex-file))
  :custom
  (latex-run-command "pdflatex")
  (tex-dvi-view-command "emacsclient -e \"(find-file-other-window \\\"*\\\")\"")
  (tex-print-file-extension ".pdf")
  (tex-run-command "pdflatex")
  :config
  (defun my-tex-file (&optional arg)
    "Save the file, except if an ARG was passed, then run `tex-file'."
    (interactive "P")
    (unless arg
      (save-buffer))
    (call-interactively #'tex-file)))


;;; tide

(use-package tide
  :after (typescript-ts-mode)
  :hook ((typescript-ts-mode-hook . eglot-ensure)
         (tsx-ts-mode-hook . eglot-ensure)))


;;; transient

(use-package transient)


;;; transpose-frame

(use-package transpose-frame
  :ensure nil
  :bind (:map my-ctl-c-t-map
              ("f" . transpose-frame)))


;;; tree-mode

(use-package tree-mode)


;;; treesit

(use-package treesit
  :demand t
  :ensure nil
  :config
  (setq treesit-language-source-alist
        ;; Language grammar downloads:
        ;; https://github.com/emacs-tree-sitter/tree-sitter-langs
        ;; https://github.com/tree-sitter-grammars
        ;; https://github.com/tree-sitter
        '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
          (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
          (json       "https://github.com/tree-sitter/tree-sitter-json")
          (mermaid    "https://github.com/monaqa/tree-sitter-mermaid")
          (nix        "https://github.com/nix-community/tree-sitter-nix")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
          (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))
  (when nil
    (mapc #'treesit-install-language-grammar
	        (mapcar #'car treesit-language-source-alist))))


;;; treesit-fold

(use-package treesit-fold
  :diminish
  :bind (:map
         my-ctl-c-f-map
         ("f" . treesit-fold-toggle)
         ("F" . treesit-fold-open-all)
         ("C" . treesit-fold-close-all)
         ("O" . treesit-fold-open-recursively))
  :config
  (global-treesit-fold-mode)
  (add-hook 'emacs-lisp-mode-hook (lambda () (treesit-parser-create 'elisp))))

(treesit-language-available-p 'bash)


;;; typescript-ts-mode

;; Installed packages for related to Typescript development:
;; tide/tsserver for LSP/eglot, prettier-js for formatting

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . typescript-ts-mode))
  :hook ((typescript-ts-mode-hook . subword-mode)
         (typescript-ts-mode-hook . no-indent-tabs-mode))
  :custom
  (typescript-ts-mode-indent-offset 2))


;;; typing

(use-package typing)


;;; unfill

(use-package unfill)


;;; uniline

(use-package uniline)


;;; uuidgen

(use-package uuidgen)


;;; visual-fill-column

(use-package visual-fill-column
  :hook ((image-mode-hook . turn-off-visual-line-fill-column-mode)
         (minibuffer-setup-hook . (lambda ()
                                    (when (minibufferp)
                                      (turn-off-visual-line-fill-column-mode))))
         (transient-setup-buffer-hook . turn-off-visual-line-fill-column-mode))
  :config
  (defun turn-off-visual-line-fill-column-mode ()
    (visual-line-fill-column-mode -1)))


;;; vundo

(use-package vundo
  :bind (("C-c u" . vundo)))


;;; winfast

(use-package winfast
  :disabled
  :ensure nil
  :bind (("M-`" . winfast-mode)))


;;; writeroom-mode

(use-package writeroom-mode
  :custom
  (writeroom-extra-line-spacing 0.2)
  (writeroom-fringes-outside-margins t)
  (writeroom-global-effects '(writeroom-set-alpha
                              writeroom-set-menu-bar-lines
                              writeroom-set-tool-bar-lines
                              writeroom-set-vertical-scroll-bars
                              writeroom-set-bottom-divider-width))
  (writeroom-maximize-window nil)
  (writeroom-width 100))


;;; xref

(use-package xref
  :ensure nil
  :bind (:map xref--xref-buffer-mode-map
              ("b" . outline-backward-same-level)
              ("f" . outline-forward-same-level))
  :hook ((xref--xref-buffer-mode-hook . outline-minor-mode)))


;;; yaml-pro

(use-package yaml-pro
  :bind (:map
         yaml-pro-ts-mode-map
         ("C-c C-x C-j" . yaml-pro-jump)
         ("C-c C-x C-p" . yaml-pro-copy-node-path-at-point)
         ("M-<down>" . yaml-pro-ts-move-subtree-down)
         ("M-<left>" . yaml-pro-ts-unindent-subtree)
         ("M-<right>" . yaml-pro-ts-indent-subtree)
         ("M-<up>" . yaml-pro-ts-move-subtree-up))
  :hook ((yaml-ts-mode-hook . yaml-pro-ts-mode)))


;;; yaml-ts-mode

(use-package yaml-ts-mode
  :ensure nil
  :hook ((yaml-ts-mode-hook . set-indent-tab-width-2))
  :mode ("\\.ya?ml\\'"))


;;; yasnippet

(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :bind (:map
         yas-minor-mode-map
         ("C-c y s d" . yas-describe-tables)
         ("C-c y s i" . yas-insert-snippet)
         ("C-c y s n" . yas-new-snippet)
         ("C-c y s r" . yas-reload-all)
         ("C-c y s s" . yas-expand)
         ("C-c y s v" . yas-visit-snippet-file)
         ("TAB" . nil))
  :custom
  (yas-prompt-functions '(yas-completing-prompt yas-no-prompt))
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  :config
  (yas-global-mode))


;;; year-calendar

(use-package year-calendar
  :demand t
  :ensure nil
  :commands (year-calendar))


;;; ztree

(use-package ztree)


(provide 'init)
;;; init.el ends here
