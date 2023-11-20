;;; init-expand-region.el --- Init customization

;;; Commentary:

;; This contains init customization for the package expand-region.

;;; Code:

(require 'expand-region)

(global-set-key (kbd "C-c e r") #'er/expand-region)
(global-set-key (kbd "C-c e i") #'er/mark-inside-pairs)
(global-set-key (kbd "C-c e o") #'er/mark-outside-pairs)
(global-set-key (kbd "C-c e s") #'er/mark-inside-quotes)
(global-set-key (kbd "C-c e S") #'er/mark-outside-quotes)

(provide 'init-expand-region)
;;; init-expand-region.el ends here
