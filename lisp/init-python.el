;;; init-python.el --- Init customization

;;; Commentary:

;; This contains init customization for programming in Python.

;;; Code:

(setq python-indent-def-block-scale 1)
;; (elpy-enable)

(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'blacken-mode)
(add-hook 'python-mode-hook #'subword-mode)

(defun pyright-make-config (selection)
  "Write pyrightconfig.json venv settings for a project."
  (interactive (list (read-directory-name "Which venv to use: "
                                          "~/.virtualenvs/"
                                          "~/.virtualenvs/"
                                          t)))
  (let ((venv-dir "~/.virtualenvs")
        (venv-name (car (last (split-string
                               (expand-file-name selection)
                               "/"
                               t))))
        (out-dir (vc-root-dir))
        (out-filename "pyrightconfig.json"))
    (if (not out-dir)
        (message "Cannot find project root")
      (with-temp-buffer

        (insert (json-encode `(:venvPath ,venv-dir :venv ,venv-name)))
        (json-pretty-print-buffer)
        (write-file (concat out-dir out-filename))
        (message "Wrote config to %s" (concat out-dir out-filename))))))

(provide 'init-python)
;;; init-python.el ends here
