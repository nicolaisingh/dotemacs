;;; init-magit.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package magit.

;;; Code:

(require 'magit)

(global-set-key (kbd "C-c g") #'magit-status)
(global-set-key (kbd "C-c M-g") #'magit-dispatch-popup)

(setq magit-repository-directories
      '(("~/prj/emacs-config/" . 0)
        ("~/prj/emacs/saveplace-pdf-view" . 0)
        ("~/prj/ideate-android/src" . 0)
        ("~/prj/masterlist-module" . 0)
        ("~/prj/nix-config" . 0)))

(setq magit-diff-highlight-hunk-body nil)

(defun magit-diff-toggle-hunk-highlights ()
  (interactive)
  (setq magit-diff-highlight-hunk-body (not magit-diff-highlight-hunk-body))
  (let ((message-log-max nil))
    (apply #'message "magit-diff-highlight-hunk-body: %s"
           `(,magit-diff-highlight-hunk-body))))

(defun magit-mode-my-custom-keys ()
  (let ((map magit-mode-map))
    (define-key map (kbd "C-c m h") #'magit-diff-toggle-hunk-highlights)))

(add-hook 'magit-mode-hook #'magit-mode-my-custom-keys)

(provide 'init-magit)
;;; init-magit.el ends here
