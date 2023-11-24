;;; init-org-modern.el --- Init customization

;;; Commentary:

;; This contains init customization for org-modern.

;;; Code:

(require 'org-modern)

(setq org-modern-tag t
      org-modern-block-name t
      org-modern-todo t
      org-modern-priority t
      org-modern-checkbox nil
      org-modern-keyword nil
      org-modern-list nil
      org-modern-progress nil
      org-modern-star nil
      org-modern-radio-target '("「" t "」"))

(defun toggle-org-modern-mode ()
  (interactive)
  (cond ((not org-modern-mode) (progn (org-modern-mode)
                                      (setq-local line-spacing 0.15)))
        (t (progn (org-modern-mode -1)
                  (setq-local line-spacing nil)))))

(define-key org-mode-map (kbd "C-c o m") #'toggle-org-modern-mode)

(set-face-attribute 'org-modern-label nil
                    :height 0.8)

(add-hook 'org-mode-hook #'toggle-org-modern-mode)

(provide 'init-org-modern)
;;; init-org-modern.el ends here
