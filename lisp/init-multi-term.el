;;; init-multi-term.el --- Init customization

;;; Commentary:

;; This contains init customization for multi-term.

;;; Code:

(require 'term)
(require 'multi-term)

(setq multi-term-program "/run/current-system/sw/bin/bash")

(global-set-key (kbd "C-c t T") #'multi-term)
(global-set-key (kbd "C-c t t") #'multi-term-next)

(provide 'init-multi-term)
;;; init-multi-term.el ends here
