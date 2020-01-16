;;; init-ivy.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package ivy.

;;; Code:

(require 'ivy)

(ivy-mode 1)

(setq-default ivy-use-virtual-buffers 1
	      ivy-count-format ""
	      ivy-height 6
	      ivy-wrap 1)

(global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x r b") 'counsel-bookmark)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h C-l") 'counsel-find-library)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h C-u") 'counsel-unicode-char)

(provide 'init-ivy)
;;; init-ivy.el ends here
