;;; init-which-key.el --- Init customization

;;; Commentary:

;; This contains init customization for the package which-key.

;;; Code:

(require 'which-key)

(setq which-key-idle-delay 1
      which-key-lighter nil)

(add-hook 'after-init-hook #'which-key-mode)

(provide 'init-which-key)
;;; init-which-key.el ends here
