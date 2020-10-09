;;; init-discover-my-major.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package discover-my-major.

;;; Code:

(require 'discover-my-major)

(global-set-key (kbd "C-c d m") #'discover-my-major)
(global-set-key (kbd "C-c d M") #'discover-my-mode)

(provide 'init-discover-my-major)
;;; init-discover-my-major.el ends here
