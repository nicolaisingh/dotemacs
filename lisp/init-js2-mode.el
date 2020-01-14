;;; init-js2-mode.el --- Init customization.

;;; Commentary:

;; This contains init customization for the package js2-mode.

;;; Code

(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'init-js2-mode)
;;; init-js2-mode.el ends here
