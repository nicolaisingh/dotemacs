;;; init-ido.el --- Init configuration.

;;; Commentary:

;; This contains init configuration for the package ido.

;;; Code:

(require 'ido)
(require 'ido-completing-read+)

(setq ido-enable-flex-matching 1
      ido-create-new-buffer 'always
      ido-use-virtual-buffers t
      ido-max-window-height 1
      ido-max-prospects 8)

(setq ido-decorations
      '("  ( "
	" ) "
	"  |  "
	"  | ..."
	"["
	"]"
	" [No match]"
	" [Matched]"
	" [Not readable]"
	" [Too big]"
	" [Confirm]"))

(setq confirm-nonexistent-file-or-buffer nil)

(ido-everywhere 1)
(ido-mode 1)
(ido-ubiquitous-mode 1)

(provide 'init-ido)
;;; init-ido.el ends here
