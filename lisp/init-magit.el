;;; init-magit.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package magit.

;;; Code:

(require 'magit)
(require 'project)

(setq magit-diff-refine-hunk t
      magit-status-goto-file-position t
      magit-status-show-hashes-in-headers t)

(defvar my-magit-global-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "g" #'magit-status)
    (keymap-set map "l" #'magit-log-current)
    (keymap-set map "M-g" #'magit-dispatch)
    map))
(keymap-global-set "C-c g" my-magit-global-map)

(keymap-set project-prefix-map "m" #'magit-project-status)
(add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
(keymap-set project-prefix-map "G" #'deadgrep)
(add-to-list 'project-switch-commands '(deadgrep "Deadgrep") t)

(setq magit-repository-directories
      '(("~/prj/emacs-config/" . 0)
        ("~/prj/emacs/saveplace-pdf-view" . 0)
        ("~/prj/ideate-android/src" . 0)
        ("~/prj/masterlist-module" . 0)
        ("~/prj/nix-config" . 0)))

(provide 'init-magit)
;;; init-magit.el ends here
