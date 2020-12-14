;;; init-smartparens.el --- Init customization

;;; Commentary:

;; This contains init customization for the package smartparens.

;;; Code:

(require 'smartparens-config)

(defun sp-forward-slurp-sexp-repeatable ()
  "Call `sp-forward-slurp-sexp' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'sp-forward-slurp-sexp))

(defun sp-backward-slurp-sexp-repeatable ()
  "Call `sp-backward-slurp-sexp' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'sp-backward-slurp-sexp))

(defun sp-forward-barf-sexp-repeatable ()
  "Call `sp-forward-barf-sexp' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'sp-forward-barf-sexp))

(defun sp-backward-barf-sexp-repeatable ()
  "Call `sp-backward-barf-sexp' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'sp-backward-barf-sexp))

(define-key smartparens-mode-map (kbd "C-c s 0") #'sp-forward-slurp-sexp-repeatable)
(define-key smartparens-mode-map (kbd "C-c s 9") #'sp-backward-slurp-sexp-repeatable)
(define-key smartparens-mode-map (kbd "C-c s )") #'sp-forward-barf-sexp-repeatable)
(define-key smartparens-mode-map (kbd "C-c s (") #'sp-backward-barf-sexp-repeatable)

(define-key smartparens-mode-map (kbd "C-c s r") #'sp-raise-sexp)
(define-key smartparens-mode-map (kbd "C-c s c") #'sp-change-enclosing)
(define-key smartparens-mode-map (kbd "C-c s j") #'sp-join-sexp)
(define-key smartparens-mode-map (kbd "C-c s s") #'sp-split-sexp)

(define-key smartparens-mode-map (kbd "M-W") #'sp-wrap-round)
(define-key smartparens-mode-map (kbd "M-U") #'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "M-F") #'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") #'sp-backward-symbol)

(define-key smartparens-mode-map (kbd "M-D") #'sp-kill-symbol)
(define-key smartparens-mode-map (kbd "M-S-<backspace>") #'sp-backward-kill-symbol)

(defun setup-smartparens-lisp ()
  (show-paren-mode)
  (smartparens-mode)
  (smartparens-strict-mode))

(add-hook 'lisp-mode-hook #'setup-smartparens-lisp)
(add-hook 'emacs-lisp-mode-hook #'setup-smartparens-lisp)
(add-hook 'clojure-mode-hook #'setup-smartparens-lisp)
(add-hook 'eval-expression-minibuffer-setup-hook #'setup-smartparens-lisp)

(add-hook 'kotlin-mode-hook #'smartparens-mode)

(provide 'init-smartparens)
;;; init-smartparens.el ends here
