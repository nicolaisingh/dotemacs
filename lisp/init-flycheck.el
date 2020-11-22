;;; init-flycheck.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package flycheck.

;;; Code:

(defun setup-kotlin ()
  (require 'flycheck-kotlin)
  (flycheck-kotlin-setup))

(eval-after-load 'flycheck #'setup-kotlin)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
