;;; init-expand-region.el --- Init customization

;;; Commentary:

;; This contains init customization for the package expand-region.

;;; Code:

(require 'expand-region)

(keymap-global-set "C-c e r" #'er/expand-region)
(keymap-global-set "C-c e i" #'er/mark-inside-pairs)
(keymap-global-set "C-c e o" #'er/mark-outside-pairs)
(keymap-global-set "C-c e s" #'er/mark-inside-quotes)
(keymap-global-set "C-c e S" #'er/mark-outside-quotes)

(provide 'init-expand-region)
;;; init-expand-region.el ends here
