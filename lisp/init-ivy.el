;;; init-ivy.el --- Init configuration

;;; Commentary:

;; This contains init configuration for the package ivy.

;;; Code:

(require 'ivy)

(ivy-mode 1)

(setq-default ivy-use-virtual-buffers 1
              ivy-count-format ""
              ivy-height 6
              ivy-wrap 1)

(keymap-global-set "M-x" 'counsel-M-x)
;; (keymap-global-set "C-x r b" 'counsel-bookmark)
(keymap-global-set "C-x C-f" 'counsel-find-file)
(keymap-global-set "C-h C-l" 'counsel-find-library)
(keymap-global-set "C-h f" 'counsel-describe-function)
(keymap-global-set "C-h v" 'counsel-describe-variable)
(keymap-global-set "C-h C-u" 'counsel-unicode-char)

(provide 'init-ivy)
;;; init-ivy.el ends here
