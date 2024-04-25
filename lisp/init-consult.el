;;; init-consult.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package consult.

;;; Code:

(require 'consult)

(setq consult-preview-key "C-SPC")

(keymap-global-set "<remap> <switch-to-buffer>" #'consult-buffer)
(keymap-global-set "<remap> <switch-to-buffer-other-frame>" #'consult-buffer-other-frame)
(keymap-global-set "<remap> <switch-to-buffer-other-window>" #'consult-buffer-other-window)
(keymap-global-set "<remap> <yank-pop>" #'consult-yank-pop)
(keymap-global-set "<remap> <repeat-complex-command>" #'consult-complex-command)
(keymap-global-set "C-c m M-x" #'consult-mode-command)

;; org-mode
(keymap-global-set "C-c o j" #'consult-org-agenda)

;; goto-map (M-g prefix)
(keymap-set goto-map "i" #'consult-imenu)
(keymap-set goto-map "I" #'consult-imenu-multi)
(keymap-set goto-map "m" #'consult-mark)
(keymap-set goto-map "M" #'consult-global-mark)
(keymap-set goto-map "h" #'consult-history)
(keymap-set goto-map "d" #'consult-dir)

;; minibuffer
(keymap-set minibuffer-local-map "M-s" #'consult-history)
(keymap-set minibuffer-local-map "M-g d" #'consult-dir)
(keymap-set minibuffer-local-map "M-g f" #'consult-dir-jump-file)

(provide 'init-consult)
;;; init-consult.el ends here
