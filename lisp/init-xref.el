;;; init-xref.el --- Init customization

;;; Commentary:

;; This contains init customization for the package xref.

;;; Code:

(defun xref-custom-keys ()
  (interactive)
  (keymap-set xref--xref-buffer-mode-map "b" #'outline-backward-same-level)
  (keymap-set xref--xref-buffer-mode-map "f" #'outline-forward-same-level))

(add-hook 'xref--xref-buffer-mode-hook #'outline-minor-mode)
(add-hook 'xref--xref-buffer-mode-hook #'xref-custom-keys)
(provide 'init-xref)
;;; init-xref.el ends here
