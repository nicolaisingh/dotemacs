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

(define-key smartparens-mode-map (kbd "C-c [ ]") #'sp-forward-slurp-sexp-repeatable)
(define-key smartparens-mode-map (kbd "C-c [ [") #'sp-backward-slurp-sexp-repeatable)
(define-key smartparens-mode-map (kbd "C-c [ }") #'sp-forward-barf-sexp-repeatable)
(define-key smartparens-mode-map (kbd "C-c [ {") #'sp-backward-barf-sexp-repeatable)

(define-key smartparens-mode-map (kbd "C-c [ s") #'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-c [ r") #'sp-raise-sexp)
(define-key smartparens-mode-map (kbd "C-c [ c") #'sp-change-enclosing)

(define-key smartparens-mode-map (kbd "M-D") #'sp-kill-symbol)
(define-key smartparens-mode-map (kbd "M-S-<backspace>") #'sp-backward-kill-symbol)

(defun setup-smartparens-lisp ()
  (show-paren-mode)
  (smartparens-mode)
  (smartparens-strict-mode))

(add-hook 'lisp-mode-hook #'setup-smartparens-lisp)
(add-hook 'emacs-lisp-mode-hook #'setup-smartparens-lisp)
(add-hook 'clojure-mode-hook #'setup-smartparens-lisp)

(add-hook 'kotlin-mode-hook #'smartparens-mode)

(provide 'init-smartparens)
;;; init-smartparens.el ends here
