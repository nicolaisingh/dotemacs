;;; aggressive-indent.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package aggressive-indent.

;;; Code:

(require 'aggressive-indent)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(global-set-key (kbd "C-c i A") #'aggressive-indent-mode)

(provide 'init-aggressive-indent)
;;; aggressive-indent.el ends here
