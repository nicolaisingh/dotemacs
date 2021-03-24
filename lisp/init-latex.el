;;; init-latex.el --- Init configuration

;;; Commentary:

;; This contains init configuration for LaTeX-related modes and
;; packages.

;;; Code:

(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; For latex-mode

(setq latex-run-command "pdflatex"
      tex-run-command "pdflatex"
      tex-print-file-extension ".pdf"
      tex-dvi-view-command "emacsclient -e \"(find-file-other-window \\\"*\\\")\"")

(provide 'init-latex)
;;; init-latex.el ends here.
