;;; window-dedicated.el --- Set windows as dedicated -*- lexical-binding: t; -*-

;;; Commentary:

;; A minor mode that toggles the `dedicated' flag of a window.

;;; Code:

(define-minor-mode window-dedicated-mode
  "Minor mode for making windows dedicated."
  :global nil
  :init-value nil
  :lighter " DEDICATED"

  (set-window-dedicated-p (get-buffer-window) window-dedicated-mode))

(provide 'window-dedicated)
;;; window-dedicated.el ends here
