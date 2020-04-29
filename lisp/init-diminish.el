;;; init-diminish.el --- Init customization.

;;; Commentary:

;; This contains init customization for the package diminish.

;;; Code:

(require 'diminish)

(defun load-diminish ()
  (eval-after-load 'org-indent
    '(diminish 'org-indent-mode))
  (eval-after-load 'subword
    '(diminish 'subword-mode "subWord"))
  (diminish 'auto-revert-mode))
(add-hook 'after-init-hook 'load-diminish)

(provide 'init-diminish)
;;; init-diminish.el ends here
