;;; init-kotlin.el --- Init configuration

;;; Commentary:

;; This contains init configuration for programming in Kotlin.
;;
;; Additional notes:
;; - Kotlin language server: https://github.com/fwcd/kotlin-language-server
;; - To detect standard library types in Kotlin (Int, String, etc.):
;;   1. Setup a local installation of gradle
;;   2. Run `gradle init' in a directory
;;   3. Run `gradle' in that directory to download the necessary libraries
;;   More info: https://github.com/fwcd/kotlin-language-server/issues/93

;;; Code:

(defun init-kotlin-lsp ()
  (require 'yasnippet)
  (require 'lsp-mode)
  (yas-minor-mode-on)
  (lsp-deferred))

(add-hook 'kotlin-mode-hook #'init-kotlin-lsp)
(add-hook 'kotlin-mode-hook #'flycheck-mode)

(provide 'init-kotlin)
;;; init-kotlin.el ends here
