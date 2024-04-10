;;; init-cov.el --- Init customization

;;; Commentary:

;; This contains init customization for cov.

;;; Code:

(require 'python)
(require 'cov)

(setq cov-coverage-mode t)

(defun cov-my-cov-mode ()
  (interactive)
  (if current-prefix-arg
      (progn
        (let ((cmd (format "cd %s; coverage json" (project-root (project-current)))))
          (call-process-shell-command cmd))
        (cov-mode 1))
    (call-interactively #'cov-mode)))

(define-key python-mode-map (kbd "C-c C C") #'cov-my-cov-mode)

(provide 'init-cov)
;;; init-cov.el ends here
