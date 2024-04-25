;;; init-calibre.el --- Init customization

;;; Commentary:

;; This contains init customization for calibre.

;;; Code:

(require 'calibre)

(setq calibre-libraries '(("library" . "~/calibre")))
(keymap-global-set "C-c L" #'calibre-library)

(provide 'init-calibre)
;;; init-calibre.el ends here
