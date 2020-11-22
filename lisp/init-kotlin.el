;;; init-kotlin.el --- Init configuration

;;; Commentary:

;; This contains init configuration for programming in Kotlin.

;;; Code:

(add-hook 'kotlin-mode-hook #'flycheck-mode)

(provide 'init-kotlin)
;;; init-kotlin.el ends here
