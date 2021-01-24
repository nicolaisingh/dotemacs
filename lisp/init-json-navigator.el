;;; init-json-navigator.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package json-navigator.

;;; Code:

(require 'json-navigator)
(require 'tree-mode)

(defun json-navigator-mode-my-custom-keys ()
  (define-key json-navigator-mode-map (kbd "SPC") #'tree-mode-expand-level)
  (define-key json-navigator-mode-map (kbd "n") #'widget-forward)
  (define-key json-navigator-mode-map (kbd "p") #'widget-backward))

(add-hook 'json-navigator-mode-hook #'json-navigator-mode-my-custom-keys)

(provide 'init-json-navigator)
;;; init-json-navigator.el ends here
