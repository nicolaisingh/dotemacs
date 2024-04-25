;;; init-json-navigator.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package json-navigator.

;;; Code:

(require 'json-navigator)
(require 'tree-mode)

(defun json-navigator-mode-my-custom-keys ()
  (keymap-set json-navigator-mode-map "SPC" #'tree-mode-expand-level)
  (keymap-set json-navigator-mode-map "n" #'widget-forward)
  (keymap-set json-navigator-mode-map "p" #'widget-backward))

(add-hook 'json-navigator-mode-hook #'json-navigator-mode-my-custom-keys)

(provide 'init-json-navigator)
;;; init-json-navigator.el ends here
