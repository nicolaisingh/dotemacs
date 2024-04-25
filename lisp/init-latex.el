;;; init-latex.el --- Init configuration

;;; Commentary:

;; This contains init configuration for LaTeX-related modes and
;; packages.

;;; Code:

(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))

;; For latex-mode

(defun tex-file-custom (&optional arg)
  (interactive "P")
  (unless arg
    (save-buffer))
  (call-interactively #'tex-file))

(setq latex-run-command "pdflatex"
      tex-run-command "pdflatex"
      tex-print-file-extension ".pdf"
      tex-dvi-view-command "emacsclient -e \"(find-file-other-window \\\"*\\\")\"")

(defun latex-mode-my-config ()
  (keymap-set latex-mode-map "C-c C-f" #'tex-file-custom))

(add-hook 'latex-mode-hook #'latex-mode-my-config)

(provide 'init-latex)
;;; init-latex.el ends here.
