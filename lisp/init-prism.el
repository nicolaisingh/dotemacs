;;; init-prism.el --- Init customization

;;; Commentary:

;; This contains init customization for the package prism.

;;; Code:

(global-set-key (kbd "C-c h b") #'prism-mode)

(add-hook 'lisp-mode-hook #'prism-mode)
(add-hook 'emacs-lisp-mode-hook #'prism-mode)
(add-hook 'clojure-mode-hook #'prism-mode)
(add-hook 'clojurescript-mode-hook #'prism-mode)
(add-hook 'scheme-mode-hook #'prism-mode)
(add-hook 'python-mode-hook #'prism-whitespace-mode)
(add-hook 'yaml-mode-hook #'prism-whitespace-mode)

(provide 'init-prism)
;;; init-prism.el ends here
