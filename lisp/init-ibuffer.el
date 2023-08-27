;;; init-ibuffer.el --- Init customization

;;; Commentary:

;; This contains init customization for the package ibuffer.

;;; Code:

(require 'ibuffer)
(require 'ibuf-ext)

(setq ibuffer-use-other-window t
      ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "nas")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
