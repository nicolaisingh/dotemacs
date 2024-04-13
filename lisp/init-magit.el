;;; init-magit.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package magit.

;;; Code:

(require 'magit)
(require 'project)

(setq magit-diff-refine-hunk t
      magit-status-goto-file-position t
      magit-status-show-hashes-in-headers t)

(defvar magit-my-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'magit-status)
    (define-key map (kbd "l") #'magit-log-current)
    (define-key map (kbd "M-g") #'magit-dispatch)
    map))
(define-key global-map (kbd "C-c g") magit-my-global-map)

(define-key project-prefix-map "m" #'magit-project-status)
(add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
(define-key project-prefix-map "G" #'deadgrep)
(add-to-list 'project-switch-commands '(deadgrep "Deadgrep") t)

(setq magit-repository-directories
      '(("~/prj/emacs-config/" . 0)
        ("~/prj/emacs/saveplace-pdf-view" . 0)
        ("~/prj/ideate-android/src" . 0)
        ("~/prj/masterlist-module" . 0)
        ("~/prj/nix-config" . 0)))

(provide 'init-magit)
;;; init-magit.el ends here
