;;; init-eshell.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package eshell.

;;; Code:

(require 'eshell)

(setq eshell-hist-ignoredups t
      eshell-history-size 10000
      eshell-ls-dired-initial-args '("-h")
      eshell-ls-initial-args '("-h")
      eshell-visual-subcommands '(("git" "log" "diff" "show" "shortlog")))

(add-to-list 'eshell-modules-list 'eshell-rebind)
(add-to-list 'eshell-modules-list 'eshell-xtra)

(defun eshell-config ()
  (setq-local completion-auto-help t)

  (keymap-set eshell-mode-map "C-c l" (lambda ()
                                        (interactive)
                                        (eshell/clear-scrollback))))
(add-hook 'eshell-mode-hook #'eshell-config)

(defun eshell-other ()
  (interactive)
  (let ((eshell-buffer-name "*eshell-other*"))
    (eshell)))

(setq eshell-names-list
      '(("nix-config" . "~/prj/nix-config")
        ("bash-scripts" . "~/prj/bash-scripts")
        ("other")))

(defun eshell-ask (eshell-name)
  (interactive (list (completing-read "Eshell: " (mapcar #'car eshell-names-list))))
  (let* ((eshell-buffer-name (concat "*" eshell-name "-eshell*"))
         (eshell-buffer (get-buffer eshell-buffer-name))
         (path-to-cd (cdr (assoc eshell-name eshell-names-list))))
    (eshell)
    (unless eshell-buffer
      (when path-to-cd
        (insert (concat "cd " path-to-cd))
        (eshell-send-input)))))

(keymap-global-set "C-c e e" #'eshell)
(keymap-global-set "C-c e 1" #'eshell-other)
(keymap-global-set "C-c e 0" #'eshell-ask)
(keymap-global-set "C-c e E" (defun eshell-new ()
                               (interactive)
                               (let ((current-prefix-arg '(4)))
                                 (call-interactively #'eshell))))

;; (setq eshell-after-prompt-hook nil)
;; esh-help
(require 'esh-help)
(setup-esh-help-eldoc)

;; esh-toggle
(require 'esh-toggle)
(keymap-global-set "C-c e t" #'eshell-toggle)

;;eshell-up
(require 'eshell-up)

;; eshell-z
(with-eval-after-load 'eshell
  (require 'eshell-z))

(provide 'init-eshell)
;;; init-eshell.el ends here
