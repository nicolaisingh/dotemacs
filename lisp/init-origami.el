;;; init-origami.el --- Init customization

;;; Commentary:

;; This contains init customization for the package origami.

;;; Code:

(require 'origami)

(defun origami-mode-my-custom-keys ()
  (keymap-set origami-mode-map "C-c f f" #'origami-toggle-node)
  (keymap-set origami-mode-map "C-c f O" #'origami-open-all-nodes)
  (keymap-set origami-mode-map "C-c f C" #'origami-close-all-nodes))

(add-hook 'origami-mode-hook #'origami-mode-my-custom-keys)
(global-origami-mode)

;;; init-origami.el ends here
(provide 'init-origami)
