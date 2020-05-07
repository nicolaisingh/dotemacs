;;; init-smartparens.el --- Init customization.

;;; Commentary:

;; This contains init customization for the package smartparens.

;;; Code:

(require 'smartparens-config)

;; Author config
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el

(define-key smartparens-mode-map (kbd "C-c } }") #'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-c { {") #'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-c ] ]") #'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-c [ [") #'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-c [ ]") #'sp-splice-sexp)

(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

(provide 'init-smartparens)
;;; init-smartparens.el ends here
