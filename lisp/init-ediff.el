;;; init-ediff.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for ediff.

;;; Code:

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(provide 'init-ediff)
;;; init-ediff.el ends here
