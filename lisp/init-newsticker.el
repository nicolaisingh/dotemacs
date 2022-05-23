;;; init-newsticker.el --- Init configuration

;;; Commentary:

;; This includes init configuration for the newsticker package.

;;; Code:

(require 'newsticker)

(defun newsticker-my-custom-keys ()
  (define-key newsticker-mode-map (kbd "<tab>") 'newsticker-show-entry))

(defun newsticker-close-buffer ()
  "Close the newsticker buffer."
  (interactive)
  ;; (newsticker--cache-update t)
  (bury-buffer))

(add-hook 'newsticker-mode-hook #'newsticker-my-custom-keys)
(add-hook 'newsticker-mode-hook #'hl-line-mode)
(add-hook 'newsticker-select-feed-hook #'reposition-window)

(provide 'init-newsticker)
;;; init-newsticker.el ends here
