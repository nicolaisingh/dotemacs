;;; init-org-present.el --- Init customization

;;; Commentary:

;; This contains init customization for org-present.

;;; Code:

(with-eval-after-load 'org-present
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-display-inline-images)
              (org-present-hide-cursor)
              (read-only-mode)))

  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (read-only-mode -1))))

(provide 'init-org-present)
;;; init-org-present.el ends here
