;;; init-transpose-frame.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package transpose-frame.

;;; Code:

(require 'transpose-frame)

(keymap-global-set "C-c t f" #'transpose-frame)

(provide 'init-transpose-frame)
;;; init-transpose-frame.el ends here
