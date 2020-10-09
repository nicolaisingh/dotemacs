;;; init-origami.el --- Init customization

;;; Commentary:

;; This contains init customization for the package origami.

;;; Code:

(require 'origami)

(define-key origami-mode-map (kbd "C-c f f") #'origami-toggle-node)
(define-key origami-mode-map (kbd "C-c f O") #'origami-open-all-nodes)
(define-key origami-mode-map (kbd "C-c f C") #'origami-close-all-nodes)

(global-origami-mode)

;;; init-origami.el ends here
(provide 'init-origami)
