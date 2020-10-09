;;; init-smartparens.el --- Init customization

;;; Commentary:

;; This contains init customization for the package smartparens.

;;; Code:

(require 'smartparens-config)

(define-key smartparens-mode-map (kbd "C-)") #'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-(") #'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-0") #'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-9") #'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-c [ ]") #'sp-splice-sexp)

(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'clojurescript-mode-hook #'smartparens-mode)

(provide 'init-smartparens)
;;; init-smartparens.el ends here
