;;; init-nav-flash.el --- Init customization

;;; Commentary:

;; This contains init customization for the package nav-flash.

;;; Code:

(require 'nav-flash)

(add-hook 'occur-mode-find-occurrence-hook #'nav-flash-show)

(provide 'init-nav-flash)
;;; init-nav-flash.el ends here
