;;; init-js2-mode.el --- Init customization

;;; Commentary:

;; This contains init customization for the package js2-mode.

;;; Code

(require 'js2-mode)
(require 'init-etc)

(defun indent-js-2-spaces ()
  (setq js-indent-level 2))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook #'indent-spaces)
(add-hook 'js2-mode-hook #'indent-js-2-spaces)
(add-hook 'js2-mode-hook #'flycheck-mode)

(provide 'init-js2-mode)
;;; init-js2-mode.el ends here
