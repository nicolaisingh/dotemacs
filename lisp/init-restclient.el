;;; init-restclient.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package restclient.

;;; Code:

(require 'restclient)

(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))

(provide 'init-restclient)
;;; init-restclient.el ends here
