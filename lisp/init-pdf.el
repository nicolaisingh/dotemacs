;;; init-pdf.el --- Init configuration

;;; Commentary:

;; This contains init configuration for PDF-related modes and
;; packages.

;;; Code:

(defun setup-pdf-tools ()
  (pdf-tools-install)
  (require 'saveplace-pdf-view))

(add-hook 'after-init-hook #'setup-pdf-tools)

(provide 'init-pdf)
;;; init-pdf.el ends here
