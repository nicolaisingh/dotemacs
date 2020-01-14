;;; init-expand-region.el --- Init customization.

;;; Commentary:

;; This contains init customization for the package expand-region.

;;; Code:

(require 'expand-region)

(global-set-key (kbd "C-c e r") 'er/expand-region)

(provide 'init-expand-region)
;;; init-expand-region.el ends here
