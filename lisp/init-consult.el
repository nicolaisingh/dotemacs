;;; init-consult.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package consult.

;;; Code:

(require 'consult)

(setq consult-preview-key "C-SPC")

(global-set-key [remap switch-to-buffer] #'consult-buffer)
(global-set-key [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
(global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
(global-set-key [remap yank-pop] #'consult-yank-pop)
(global-set-key [remap repeat-complex-command] #'consult-complex-command)
(global-set-key (kbd "C-c m M-x") #'consult-mode-command)

;; org-mode
(global-set-key (kbd "C-c o j") #'consult-org-agenda)

;; goto-map (M-g prefix)
(define-key goto-map (kbd "i") #'consult-imenu)
(define-key goto-map (kbd "I") #'consult-imenu-multi)
(define-key goto-map (kbd "m") #'consult-mark)
(define-key goto-map (kbd "M") #'consult-global-mark)
(define-key goto-map (kbd "h") #'consult-history)

;; minibuffer
(define-key minibuffer-local-map (kbd "M-s") #'consult-history)

(provide 'init-consult)
;;; init-consult.el ends here
