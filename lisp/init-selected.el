;;; init-selected.el --- Init customization

;;; Commentary:

;; This contains init customization for selected.

;;; Code:

(require 'selected)

(let ((map selected-keymap))
  (define-key map (kbd "q") #'selected-off)
  (define-key map (kbd "C") #'capitalize-region)
  (define-key map (kbd "F") #'flush-lines)
  (define-key map (kbd "K") #'keep-lines)
  (define-key map (kbd "SPC") #'canonically-space-region)
  (define-key map (kbd "f") #'fill-region)
  (define-key map (kbd "r") #'reverse-region)
  (define-key map (kbd "s") #'sort-lines)
  (define-key map (kbd "u") #'unfill-region))

(defun turn-off-selected-minor-mode ()
  (selected-minor-mode -1))

(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook #'turn-off-selected-minor-mode))

(selected-global-mode 1)

(provide 'init-selected)
;;; init-selected.el ends here
