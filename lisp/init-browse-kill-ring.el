;;; init-browse-kill-ring.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package browse-kill-ring.

;;; Code:

(require 'browse-kill-ring)

(keymap-global-set "C-c b k" #'browse-kill-ring)

(provide 'init-browse-kill-ring)
;;; init-browse-kill-ring.el ends here
