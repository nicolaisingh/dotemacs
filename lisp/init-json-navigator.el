;;; init-json-navigator.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package json-navigator.

;;; Code:

(require 'json-navigator)
(require 'tree-mode)

(define-key json-navigator-mode-map (kbd "SPC") #'tree-mode-expand-level)
(define-key json-navigator-mode-map (kbd "n") #'widget-forward)
(define-key json-navigator-mode-map (kbd "p") #'widget-backward)

(provide 'init-json-navigator)
;;; init-json-navigator.el ends here
