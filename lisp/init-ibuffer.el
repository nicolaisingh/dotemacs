;;; init-ibuffer.el --- Init customization

;;; Commentary:

;; This contains init customization for the package ibuffer.

;;; Code:

(require 'ibuffer)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "nas")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
