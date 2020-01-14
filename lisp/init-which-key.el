;;; init-which-key.el --- Init customization.

;;; Commentary:

;; This contains init customization for the package which-key.

;;; Code:

(require 'which-key)

(setq-default which-key-idle-delay 2)
(setq-default which-key-lighter nil)

(add-hook 'after-init-hook #'which-key-mode)

(provide 'init-which-key)
;;; init-which-key.el ends here
