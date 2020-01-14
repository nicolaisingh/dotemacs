;;; init-latex.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for LaTeX-related modes and
;; packages.

;;; Code:

;; (pdf-tools-install)
(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))

(add-hook 'after-init-hook 'pdf-tools-install)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(provide 'init-latex)
;;; init-latex.el ends here.
