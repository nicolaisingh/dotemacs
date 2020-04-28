;;; init-transpose-frame.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package transpose-frame.

;;; Code:

(require 'transpose-frame)

(global-set-key (kbd "C-c t f") 'transpose-frame)
(global-set-key (kbd "C-~") 'transpose-frame)

(provide 'init-transpose-frame)
;;; init-transpose-frame.el ends here
