;;; init-smartparens.el --- Init customization

;;; Commentary:

;; This contains init customization for the package smartparens.

;;; Code:

(require 'smartparens-config)

(defun sp-repeatable-extra-bindings ()
  `(("0" . ,(if (memq major-mode sp-lisp-modes)
                #'sp-forward-slurp-sexp-repeatable
              #'sp-slurp-hybrid-sexp-repeatable))
    ("9" . sp-backward-slurp-sexp-repeatable)
    (")" . sp-forward-barf-sexp-repeatable)
    ("(" . sp-backward-barf-sexp-repeatable)))

(defun sp-forward-slurp-sexp-repeatable ()
  "Call `sp-forward-slurp-sexp' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'sp-forward-slurp-sexp (sp-repeatable-extra-bindings)))

(defun sp-backward-slurp-sexp-repeatable ()
  "Call `sp-backward-slurp-sexp' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'sp-backward-slurp-sexp (sp-repeatable-extra-bindings)))

(defun sp-forward-barf-sexp-repeatable ()
  "Call `sp-forward-barf-sexp' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'sp-forward-barf-sexp (sp-repeatable-extra-bindings)))

(defun sp-backward-barf-sexp-repeatable ()
  "Call `sp-backward-barf-sexp' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'sp-backward-barf-sexp (sp-repeatable-extra-bindings)))

(defun sp-slurp-hybrid-sexp-repeatable ()
  "Call `sp-slurp-hybrid-sexp' using `repeatkey-repeatable-call'."
  (interactive)
  (repeatkey-repeatable-call #'sp-slurp-hybrid-sexp (sp-repeatable-extra-bindings)))

(defun smartparens-mode-my-custom-keys ()
  (if (memq major-mode sp-lisp-modes)
      (define-key smartparens-mode-map (kbd "C-c s 0") #'sp-forward-slurp-sexp-repeatable)
    (define-key smartparens-mode-map (kbd "C-c s 0") #'sp-slurp-hybrid-sexp-repeatable))
  (define-key smartparens-mode-map (kbd "C-c s 9") #'sp-backward-slurp-sexp-repeatable)
  (define-key smartparens-mode-map (kbd "C-c s )") #'sp-forward-barf-sexp-repeatable)
  (define-key smartparens-mode-map (kbd "C-c s (") #'sp-backward-barf-sexp-repeatable)

  (define-key smartparens-mode-map (kbd "C-c s a") #'sp-absorb-sexp)
  (define-key smartparens-mode-map (kbd "C-c s e") #'sp-emit-sexp)
  (define-key smartparens-mode-map (kbd "C-c s r") #'sp-raise-sexp)
  (define-key smartparens-mode-map (kbd "C-c s c") #'sp-change-inner)
  (define-key smartparens-mode-map (kbd "C-c s j") #'sp-join-sexp)
  (define-key smartparens-mode-map (kbd "C-c s s") #'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-c s S") #'sp-split-sexp)

  (define-key smartparens-mode-map (kbd "M-W") #'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "M-U") #'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-F") #'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "M-B") #'sp-backward-symbol)

  (define-key smartparens-mode-map (kbd "M-D") #'sp-kill-symbol)
  (define-key smartparens-mode-map (kbd "M-S-<backspace>") #'sp-backward-kill-symbol)

  (define-key smartparens-mode-map (kbd "C-M-<backspace>") #'backward-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-f") #'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") #'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-n") #'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") #'sp-previous-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") #'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-d") #'sp-down-sexp))

(defun setup-smartparens-lisp ()
  (show-paren-local-mode)
  (smartparens-mode)
  (smartparens-strict-mode))

(add-hook 'lisp-mode-hook #'setup-smartparens-lisp)
(add-hook 'emacs-lisp-mode-hook #'setup-smartparens-lisp)
(add-hook 'clojure-mode-hook #'setup-smartparens-lisp)
(add-hook 'scheme-mode-hook #'setup-smartparens-lisp)
(add-hook 'eval-expression-minibuffer-setup-hook #'setup-smartparens-lisp)

(add-hook 'kotlin-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'latex-mode-hook #'smartparens-mode)
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'org-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-strict-mode)

(add-hook 'smartparens-mode-hook #'smartparens-mode-my-custom-keys)

(provide 'init-smartparens)
;;; init-smartparens.el ends here
